// Methods for type_info for -*- C++ -*- Run Time Type Identification.
// Copyright (C) 1994, 1996, 1998, 1999, 2000 Free Software Foundation

// This file is part of GNU CC.

// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA. 

// As a special exception, if you link this library with other files,
// some of which are compiled with GCC, to produce an executable,
// this library does not by itself cause the resulting executable
// to be covered by the GNU General Public License.
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.

#pragma implementation "typeinfo"

#include <stddef.h>
#include "tinfo.h"
#include "new"			// for placement new

// This file contains the minimal working set necessary to link with code
// that uses virtual functions and -frtti but does not actually use RTTI
// functionality.

std::type_info::
~type_info ()
{ }

#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
// original (old) abi

namespace
{
// ADDR is a pointer to an object.  Convert it to a pointer to a base,
// using OFFSET.
inline void*
convert_to_base (void *addr, bool is_virtual, myint32 offset)
{
  if (!addr)
    return NULL;

  if (!is_virtual)
    return (char *) addr + offset;

  // Under the old ABI, the offset gives us the address of a pointer
  // to the virtual base.
  return *((void **) ((char *) addr + offset));
}

}

// We can't rely on common symbols being shared between shared objects.
bool std::type_info::
operator== (const std::type_info& arg) const
{
  return (&arg == this) || (__builtin_strcmp (name (), arg.name ()) == 0);
}

extern "C" void
__rtti_class (void *addr, const char *name,
	      const __class_type_info::base_info *bl, size_t bn)
{ new (addr) __class_type_info (name, bl, bn); }

extern "C" void
__rtti_si (void *addr, const char *n, const std::type_info *ti)
{
  new (addr) __si_type_info
    (n, static_cast <const __user_type_info &> (*ti));
}

extern "C" void
__rtti_user (void *addr, const char *name)
{ new (addr) __user_type_info (name); }

// Upcast for catch checking. OBJPTR points to the thrown object and might be
// NULL. Return 0 on failure, non-zero on success. Set *ADJPTR to adjusted
// object pointer.
int __user_type_info::
upcast (const type_info &target, void *objptr,
        void **adjptr) const
{
  upcast_result result;
  
  if (do_upcast (contained_public, target, objptr, result))
    return 0;
  *adjptr = result.target_obj;
  return contained_public_p (result.whole2target);
}

// Down or cross cast for dynamic_cast. OBJPTR points to the most derrived
// object, SUBPTR points to the static base object. Both must not be NULL.
// TARGET specifies the desired target type, SUBTYPE specifies the static
// type. Both must be defined. Returns adjusted object pointer on success,
// NULL on failure. [expr.dynamic.cast]/8 says 'unambiguous public base'. This
// itself is an ambiguous statement. We choose it to mean the base must be
// separately unambiguous and public, rather than unambiguous considering only
// public bases.
void *__user_type_info::
dyncast (int boff,
         const type_info &target, void *objptr,
         const type_info &subtype, void *subptr) const
{
  dyncast_result result;
  
  do_dyncast (boff, contained_public,
              target, objptr, subtype, subptr, result);
  if (!result.target_obj)
    return NULL;
  if (contained_public_p (result.target2sub))
    return result.target_obj;
  if (contained_public_p (sub_kind (result.whole2sub & result.whole2target)))
    // Found a valid cross cast
    return result.target_obj;
  if (contained_nonvirtual_p (result.whole2sub))
    // Found an invalid cross cast, which cannot also be a down cast
    return NULL;
  if (result.target2sub == unknown)
    result.target2sub = static_cast <const __user_type_info &> (target)
                        .find_public_subobj (boff, subtype,
                                             result.target_obj, subptr);
  if (contained_public_p (result.target2sub))
    // Found a valid down cast
    return result.target_obj;
  // Must be an invalid down cast, or the cross cast wasn't bettered
  return NULL;
}

// Catch cast helper. ACCESS_PATH is the access from the complete thrown
// object to this base. TARGET is the desired type we want to catch. OBJPTR
// points to this base within the throw object, it might be NULL. Fill in
// RESULT with what we find. Return true, should we determine catch must fail.
bool __user_type_info::
do_upcast (sub_kind access_path,
           const type_info &target, void *objptr,
           upcast_result &__restrict result) const
{
  if (*this == target)
    {
      result.target_obj = objptr;
      result.base_type = nonvirtual_base_type;
      result.whole2target = access_path;
      return contained_nonpublic_p (access_path);
    }
  return false;
}

// dynamic cast helper. ACCESS_PATH gives the access from the most derived
// object to this base. TARGET indicates the desired type we want. OBJPTR
// points to this base within the object. SUBTYPE indicates the static type
// started from and SUBPTR points to that base within the most derived object.
// Fill in RESULT with what we find. Return true if we have located an
// ambiguous match.
bool __user_type_info::
do_dyncast (int, sub_kind access_path,
            const type_info &target, void *objptr,
            const type_info &subtype, void *subptr,
            dyncast_result &__restrict result) const
{
  if (objptr == subptr && *this == subtype)
    {
      // The subobject we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2sub = access_path;
      return false;
    }
  if (*this == target)
    {
      result.target_obj = objptr;
      result.whole2target = access_path;
      result.target2sub = not_contained;
      return false;
    }
  return false;
}

// find_public_subobj helper. Return contained_public if we are the desired
// subtype. OBJPTR points to this base type, SUBPTR points to the desired base
// object.
__user_type_info::sub_kind __user_type_info::
do_find_public_subobj (int, const type_info &, void *objptr, void *subptr) const
{
  if (subptr == objptr)
    // Must be our type, as the pointers match.
    return contained_public;
  return not_contained;
}

// catch helper for single public inheritance types. See
// __user_type_info::do_upcast for semantics.
bool __si_type_info::
do_upcast (sub_kind access_path,
           const type_info &target, void *objptr,
           upcast_result &__restrict result) const
{
  if (*this == target)
    {
      result.target_obj = objptr;
      result.base_type = nonvirtual_base_type;
      result.whole2target = access_path;
      return contained_nonpublic_p (access_path);
    }
  return base.do_upcast (access_path, target, objptr, result);
}

// dynamic cast helper for single public inheritance types. See
// __user_type_info::do_dyncast for semantics. BOFF indicates how SUBTYPE
// types are inherited by TARGET types.
bool __si_type_info::
do_dyncast (int boff, sub_kind access_path,
            const type_info &target, void *objptr,
            const type_info &subtype, void *subptr,
            dyncast_result &__restrict result) const
{
  if (objptr == subptr && *this == subtype)
    {
      // The subobject we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2sub = access_path;
      return false;
    }
  if (*this == target)
    {
      result.target_obj = objptr;
      result.whole2target = access_path;
      if (boff >= 0)
        result.target2sub = ((char *)subptr - (char *)objptr) == boff
              ? contained_public : not_contained;
      else if (boff == -2)
        result.target2sub = not_contained;
      return false;
    }
  return base.do_dyncast (boff, access_path,
                          target, objptr, subtype, subptr, result);
}

// find_public_subobj helper. See __user_type_info::do_find_public_subobj or
// semantics. BOFF indicates how SUBTYPE types are inherited by the original
// target object.
__user_type_info::sub_kind __si_type_info::
do_find_public_subobj (int boff, const type_info &subtype, void *objptr, void *subptr) const
{
  if (subptr == objptr && subtype == *this)
    return contained_public;
  return base.do_find_public_subobj (boff, subtype, objptr, subptr);
}

// catch helper for multiple or non-public inheritance types. See
// __user_type_info::do_upcast for semantics.
bool __class_type_info::
do_upcast (sub_kind access_path,
           const type_info &target, void *objptr,
           upcast_result &__restrict result) const
{
  if (*this == target)
    {
      result.target_obj = objptr;
      result.base_type = nonvirtual_base_type;
      result.whole2target = access_path;
      return contained_nonpublic_p (access_path);
    }
  
  for (size_t i = n_bases; i--;)
    {
      upcast_result result2;
      void *p = objptr;
      sub_kind sub_access = access_path;
      p = convert_to_base (p, 
			   base_list[i].is_virtual,
			   base_list[i].offset);
      if (base_list[i].is_virtual)
	sub_access = sub_kind (sub_access | contained_virtual_mask);
      if (base_list[i].access != PUBLIC)
        sub_access = sub_kind (sub_access & ~contained_public_mask);
      if (base_list[i].base->do_upcast (sub_access, target, p, result2))
        return true; // must fail
      if (result2.base_type)
        {
          if (result2.base_type == nonvirtual_base_type
              && base_list[i].is_virtual)
            result2.base_type = base_list[i].base;
          if (!result.base_type)
            result = result2;
          else if (result.target_obj != result2.target_obj)
            {
              // Found an ambiguity.
	      result.target_obj = NULL;
	      result.whole2target = contained_ambig;
	      return true;
            }
          else if (result.target_obj)
            {
              // Ok, found real object via a virtual path.
              result.whole2target
                  = sub_kind (result.whole2target | result2.whole2target);
            }
          else
            {
              // Dealing with a null pointer, need to check vbase
              // containing each of the two choices.
              if (result2.base_type == nonvirtual_base_type
                  || result.base_type == nonvirtual_base_type
                  || !(*result2.base_type == *result.base_type))
                {
                  // Already ambiguous, not virtual or via different virtuals.
                  // Cannot match.
                  result.whole2target = contained_ambig;
                  return true;
                }
            }
        }
    }
  return false;
}

// dynamic cast helper for non-public or multiple inheritance types. See
// __user_type_info::do_dyncast for overall semantics.
// This is a big hairy function. Although the run-time behaviour of
// dynamic_cast is simple to describe, it gives rise to some non-obvious
// behaviour. We also desire to determine as early as possible any definite
// answer we can get. Because it is unknown what the run-time ratio of
// succeeding to failing dynamic casts is, we do not know in which direction
// to bias any optimizations. To that end we make no particular effort towards
// early fail answers or early success answers. Instead we try to minimize
// work by filling in things lazily (when we know we need the information),
// and opportunisticly take early success or failure results.
bool __class_type_info::
do_dyncast (int boff, sub_kind access_path,
            const type_info &target, void *objptr,
            const type_info &subtype, void *subptr,
            dyncast_result &__restrict result) const
{
  if (objptr == subptr && *this == subtype)
    {
      // The subobject we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2sub = access_path;
      return false;
    }
  if (*this == target)
    {
      result.target_obj = objptr;
      result.whole2target = access_path;
      if (boff >= 0)
        result.target2sub = ((char *)subptr - (char *)objptr) == boff
              ? contained_public : not_contained;
      else if (boff == -2)
        result.target2sub = not_contained;
      return false;
    }
  bool result_ambig = false;
  for (size_t i = n_bases; i--;)
    {
      dyncast_result result2;
      void *p;
      sub_kind sub_access = access_path;
      p = convert_to_base (objptr, 
			   base_list[i].is_virtual,
			   base_list[i].offset);
      if (base_list[i].is_virtual)
	sub_access = sub_kind (sub_access | contained_virtual_mask);
      if (base_list[i].access != PUBLIC)
        sub_access = sub_kind (sub_access & ~contained_public_mask);
      
      bool result2_ambig
          = base_list[i].base->do_dyncast (boff, sub_access,
                                           target, p, subtype, subptr, result2);
      result.whole2sub = sub_kind (result.whole2sub | result2.whole2sub);
      if (result2.target2sub == contained_public
          || result2.target2sub == contained_ambig)
        {
          result.target_obj = result2.target_obj;
          result.whole2target = result2.whole2target;
          result.target2sub = result2.target2sub;
          // Found a downcast which can't be bettered or an ambiguous downcast
          // which can't be disambiguated
          return result2_ambig;
        }
      
      if (!result_ambig && !result.target_obj)
        {
          // Not found anything yet.
          result.target_obj = result2.target_obj;
          result.whole2target = result2.whole2target;
          result_ambig = result2_ambig;
        }
      else if (result.target_obj && result.target_obj == result2.target_obj)
        {
          // Found at same address, must be via virtual.  Pick the most
          // accessible path.
          result.whole2target =
              sub_kind (result.whole2target | result2.whole2target);
        }
      else if ((result.target_obj && result2.target_obj)
               || (result_ambig && result2.target_obj)
               || (result2_ambig && result.target_obj))
        {
          // Found two different TARGET bases, or a valid one and a set of
          // ambiguous ones, must disambiguate. See whether SUBOBJ is
          // contained publicly within one of the non-ambiguous choices.
          // If it is in only one, then that's the choice. If it is in
          // both, then we're ambiguous and fail. If it is in neither,
          // we're ambiguous, but don't yet fail as we might later find a
          // third base which does contain SUBPTR.
        
          sub_kind new_sub_kind = result2.target2sub;
          sub_kind old_sub_kind = result.target2sub;
          
          if (contained_nonvirtual_p (result.whole2sub))
            {
              // We already found SUBOBJ as a non-virtual base of most
              // derived. Therefore if it is in either choice, it can only be
              // in one of them, and we will already know.
              if (old_sub_kind == unknown)
                old_sub_kind = not_contained;
              if (new_sub_kind == unknown)
                new_sub_kind = not_contained;
            }
          else
            {
              const __user_type_info &t =
                  static_cast <const __user_type_info &> (target);
              
              if (old_sub_kind >= not_contained)
                ;// already calculated
              else if (contained_nonvirtual_p (new_sub_kind))
                // Already found non-virtually inside the other choice,
                // cannot be in this.
                old_sub_kind = not_contained;
              else
                old_sub_kind = t.find_public_subobj (boff, subtype,
                                                     result.target_obj, subptr);
          
              if (new_sub_kind >= not_contained)
                ;// already calculated
              else if (contained_nonvirtual_p (old_sub_kind))
                // Already found non-virtually inside the other choice,
                // cannot be in this.
                new_sub_kind = not_contained;
              else
                new_sub_kind = t.find_public_subobj (boff, subtype,
                                                     result2.target_obj, subptr);
            }
          
          // Neither sub_kind can be contained_ambig -- we bail out early
          // when we find those.
          if (contained_p (sub_kind (new_sub_kind ^ old_sub_kind)))
            {
              // Only on one choice, not ambiguous.
              if (contained_p (new_sub_kind))
                {
                  // Only in new.
                  result.target_obj = result2.target_obj;
                  result.whole2target = result2.whole2target;
                  result_ambig = false;
                  old_sub_kind = new_sub_kind;
                }
              result.target2sub = old_sub_kind;
              if (result.target2sub == contained_public)
                return false; // Can't be an ambiguating downcast for later discovery.
            }
          else if (contained_p (sub_kind (new_sub_kind & old_sub_kind)))
            {
              // In both.
              result.target_obj = NULL;
              result.target2sub = contained_ambig;
              return true;  // Fail.
            }
          else
            {
              // In neither publicly, ambiguous for the moment, but keep
              // looking. It is possible that it was private in one or
              // both and therefore we should fail, but that's just tough.
              result.target_obj = NULL;
              result.target2sub = not_contained;
              result_ambig = true;
            }
        }
      
      if (result.whole2sub == contained_private)
        // We found SUBOBJ as a private non-virtual base, therefore all
        // cross casts will fail. We have already found a down cast, if
        // there is one.
        return result_ambig;
    }

  return result_ambig;
}

// find_public_subobj helper for non-public or multiple inheritance types. See
// __user_type_info::do_find_public_subobj for semantics. We make use of BOFF
// to prune the base class walk.
__user_type_info::sub_kind __class_type_info::
do_find_public_subobj (int boff, const type_info &subtype, void *objptr, void *subptr) const
{
  if (objptr == subptr && subtype == *this)
    return contained_public;
  
  for (size_t i = n_bases; i--;)
    {
      if (base_list[i].access != PUBLIC)
        continue; // Not public, can't be here.
      void *p;

      if (base_list[i].is_virtual && boff == -3)
	// Not a virtual base, so can't be here.
	continue;
      
      p = convert_to_base (objptr, 
			   base_list[i].is_virtual,
			   base_list[i].offset);

      sub_kind base_kind = base_list[i].base->do_find_public_subobj
                              (boff, subtype, p, subptr);
      if (contained_p (base_kind))
        {
          if (base_list[i].is_virtual)
            base_kind = sub_kind (base_kind | contained_virtual_mask);
          return base_kind;
        }
    }
  
  return not_contained;
}
#else
// new abi

namespace std {

// return true if this is a type_info for a pointer type
bool type_info::
__is_pointer_p () const
{
  return false;
}

// return true if this is a type_info for a function type
bool type_info::
__is_function_p () const
{
  return false;
}

// try and catch a thrown object.
bool type_info::
__do_catch (const type_info *thr_type, void **, unsigned) const
{
  return *this == *thr_type;
}

// upcast from this type to the target. __class_type_info will override
bool type_info::
__do_upcast (const abi::__class_type_info *, void **) const
{
  return false;
}

};

namespace {

using namespace std;
using namespace abi;

// initial part of a vtable, this structure is used with offsetof, so we don't
// have to keep alignments consistent manually.
struct vtable_prefix {
  ptrdiff_t whole_object;           // offset to most derived object
  const __class_type_info *whole_type;  // pointer to most derived type_info
  const void *origin;               // what a class's vptr points to
};

template <typename T>
inline const T *
adjust_pointer (const void *base, ptrdiff_t offset)
{
  return reinterpret_cast <const T *>
    (reinterpret_cast <const char *> (base) + offset);
}

// ADDR is a pointer to an object.  Convert it to a pointer to a base,
// using OFFSET. IS_VIRTUAL is true, if we are getting a virtual base.
inline void const *
convert_to_base (void const *addr, bool is_virtual, ptrdiff_t offset)
{
  if (is_virtual)
    {
      const void *vtable = *static_cast <const void *const *> (addr);
      
      offset = *adjust_pointer<ptrdiff_t> (vtable, offset);
    }

  return adjust_pointer<void> (addr, offset);
}

// some predicate functions for __class_type_info::__sub_kind
inline bool contained_p (__class_type_info::__sub_kind access_path)
{
  return access_path >= __class_type_info::__contained_mask;
}
inline bool public_p (__class_type_info::__sub_kind access_path)
{
  return access_path & __class_type_info::__contained_public_mask;
}
inline bool virtual_p (__class_type_info::__sub_kind access_path)
{
  return (access_path & __class_type_info::__contained_virtual_mask);
}
inline bool contained_public_p (__class_type_info::__sub_kind access_path)
{
  return ((access_path & __class_type_info::__contained_public)
          == __class_type_info::__contained_public);
}
inline bool contained_nonpublic_p (__class_type_info::__sub_kind access_path)
{
  return ((access_path & __class_type_info::__contained_public)
          == __class_type_info::__contained_mask);
}
inline bool contained_nonvirtual_p (__class_type_info::__sub_kind access_path)
{
  return ((access_path & (__class_type_info::__contained_mask
                          | __class_type_info::__contained_virtual_mask))
          == __class_type_info::__contained_mask);
}

static const __class_type_info *const nonvirtual_base_type =
    static_cast <const __class_type_info *> (0) + 1;

}; // namespace

namespace __cxxabiv1
{

__class_type_info::
~__class_type_info ()
{}

__si_class_type_info::
~__si_class_type_info ()
{}

__vmi_class_type_info::
~__vmi_class_type_info ()
{}

// __upcast_result is used to hold information during traversal of a class
// heirarchy when catch matching.
struct __class_type_info::__upcast_result
{
  const void *dst_ptr;        // pointer to caught object
  __sub_kind whole2dst;       // path from most derived object to target
  int src_details;            // hints about the source type heirarchy
  const __class_type_info *base_type; // where we found the target,
                              // if in vbase the __class_type_info of vbase
                              // if a non-virtual base then 1
                              // else NULL
  public:
  __upcast_result (int d)
    :dst_ptr (NULL), whole2dst (__unknown), src_details (d), base_type (NULL)
    {}
};

// __dyncast_result is used to hold information during traversal of a class
// heirarchy when dynamic casting.
struct __class_type_info::__dyncast_result
{
  const void *dst_ptr;        // pointer to target object or NULL
  __sub_kind whole2dst;       // path from most derived object to target
  __sub_kind whole2src;       // path from most derived object to sub object
  __sub_kind dst2src;         // path from target to sub object
  
  public:
  __dyncast_result ()
    :dst_ptr (NULL), whole2dst (__unknown),
     whole2src (__unknown), dst2src (__unknown)
    {}
};

bool __class_type_info::
__do_catch (const type_info *thr_type,
            void **thr_obj,
            unsigned outer) const
{
  if (*this == *thr_type)
    return true;
  if (outer >= 4)
    // Neither `A' nor `A *'.
    return false;
  return thr_type->__do_upcast (this, thr_obj);
}

bool __class_type_info::
__do_upcast (const __class_type_info *dst_type,
             void **obj_ptr) const
{
  __upcast_result result (__vmi_class_type_info::__flags_unknown_mask);
  
  if (__do_upcast (__contained_public, dst_type, *obj_ptr, result))
    return false;
  *obj_ptr = const_cast <void *> (result.dst_ptr);
  return contained_public_p (result.whole2dst);
}

inline __class_type_info::__sub_kind __class_type_info::
__find_public_src (ptrdiff_t src2dst,
                   const void *obj_ptr,
                   const __class_type_info *src_type,
                   const void *src_ptr) const
{
  if (src2dst >= 0)
    return adjust_pointer <void> (obj_ptr, src2dst) == src_ptr
            ? __contained_public : __not_contained;
  if (src2dst == -2)
    return __not_contained;
  return __do_find_public_src (src2dst, obj_ptr, src_type, src_ptr);
}

__class_type_info::__sub_kind __class_type_info::
__do_find_public_src (ptrdiff_t,
                      const void *obj_ptr,
                      const __class_type_info *,
                      const void *src_ptr) const
{
  if (src_ptr == obj_ptr)
    // Must be our type, as the pointers match.
    return __contained_public;
  return __not_contained;
}

__class_type_info::__sub_kind __si_class_type_info::
__do_find_public_src (ptrdiff_t src2dst,
                      const void *obj_ptr,
                      const __class_type_info *src_type,
                      const void *src_ptr) const
{
  if (src_ptr == obj_ptr && *this == *src_type)
    return __contained_public;
  return base->__do_find_public_src (src2dst, obj_ptr, src_type, src_ptr);
}

__class_type_info::__sub_kind __vmi_class_type_info::
__do_find_public_src (ptrdiff_t src2dst,
                      const void *obj_ptr,
                      const __class_type_info *src_type,
                      const void *src_ptr) const
{
  if (obj_ptr == src_ptr && *this == *src_type)
    return __contained_public;
  
  for (size_t i = vmi_base_count; i--;)
    {
      if (!vmi_bases[i].__is_public_p ())
        continue; // Not public, can't be here.
      
      const void *base = obj_ptr;
      ptrdiff_t offset = vmi_bases[i].__offset ();
      bool is_virtual = vmi_bases[i].__is_virtual_p ();
      
      if (is_virtual)
        {
          if (src2dst == -3)
            continue; // Not a virtual base, so can't be here.
        }
      base = convert_to_base (base, is_virtual, offset);
      
      __sub_kind base_kind = vmi_bases[i].base->__do_find_public_src
                              (src2dst, base, src_type, src_ptr);
      if (contained_p (base_kind))
        {
          if (is_virtual)
            base_kind = __sub_kind (base_kind | __contained_virtual_mask);
          return base_kind;
        }
    }
  
  return __not_contained;
}

bool __class_type_info::
__do_dyncast (ptrdiff_t,
              __sub_kind access_path,
              const __class_type_info *dst_type,
              const void *obj_ptr,
              const __class_type_info *src_type,
              const void *src_ptr,
              __dyncast_result &__restrict result) const
{
  if (obj_ptr == src_ptr && *this == *src_type)
    {
      // The src object we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2src = access_path;
      return false;
    }
  if (*this == *dst_type)
    {
      result.dst_ptr = obj_ptr;
      result.whole2dst = access_path;
      result.dst2src = __not_contained;
      return false;
    }
  return false;
}

bool __si_class_type_info::
__do_dyncast (ptrdiff_t src2dst,
              __sub_kind access_path,
              const __class_type_info *dst_type,
              const void *obj_ptr,
              const __class_type_info *src_type,
              const void *src_ptr,
              __dyncast_result &__restrict result) const
{
  if (*this == *dst_type)
    {
      result.dst_ptr = obj_ptr;
      result.whole2dst = access_path;
      if (src2dst >= 0)
        result.dst2src = adjust_pointer <void> (obj_ptr, src2dst) == src_ptr
              ? __contained_public : __not_contained;
      else if (src2dst == -2)
        result.dst2src = __not_contained;
      return false;
    }
  if (obj_ptr == src_ptr && *this == *src_type)
    {
      // The src object we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2src = access_path;
      return false;
    }
  return base->__do_dyncast (src2dst, access_path, dst_type, obj_ptr,
                             src_type, src_ptr, result);
}

// This is a big hairy function. Although the run-time behaviour of
// dynamic_cast is simple to describe, it gives rise to some non-obvious
// behaviour. We also desire to determine as early as possible any definite
// answer we can get. Because it is unknown what the run-time ratio of
// succeeding to failing dynamic casts is, we do not know in which direction
// to bias any optimizations. To that end we make no particular effort towards
// early fail answers or early success answers. Instead we try to minimize
// work by filling in things lazily (when we know we need the information),
// and opportunisticly take early success or failure results.
bool __vmi_class_type_info::
__do_dyncast (ptrdiff_t src2dst,
              __sub_kind access_path,
              const __class_type_info *dst_type,
              const void *obj_ptr,
              const __class_type_info *src_type,
              const void *src_ptr,
              __dyncast_result &__restrict result) const
{
  if (obj_ptr == src_ptr && *this == *src_type)
    {
      // The src object we started from. Indicate how we are accessible from
      // the most derived object.
      result.whole2src = access_path;
      return false;
    }
  if (*this == *dst_type)
    {
      result.dst_ptr = obj_ptr;
      result.whole2dst = access_path;
      if (src2dst >= 0)
        result.dst2src = adjust_pointer <void> (obj_ptr, src2dst) == src_ptr
              ? __contained_public : __not_contained;
      else if (src2dst == -2)
        result.dst2src = __not_contained;
      return false;
    }
  bool result_ambig = false;
  for (size_t i = vmi_base_count; i--;)
    {
      __dyncast_result result2;
      void const *base = obj_ptr;
      __sub_kind base_access = access_path;
      ptrdiff_t offset = vmi_bases[i].__offset ();
      bool is_virtual = vmi_bases[i].__is_virtual_p ();
      
      if (is_virtual)
        base_access = __sub_kind (base_access | __contained_virtual_mask);
      base = convert_to_base (base, is_virtual, offset);

      if (!vmi_bases[i].__is_public_p ())
        base_access = __sub_kind (base_access & ~__contained_public_mask);
      
      bool result2_ambig
          = vmi_bases[i].base->__do_dyncast (src2dst, base_access,
                                             dst_type, base,
                                             src_type, src_ptr, result2);
      result.whole2src = __sub_kind (result.whole2src | result2.whole2src);
      if (result2.dst2src == __contained_public
          || result2.dst2src == __contained_ambig)
        {
          result.dst_ptr = result2.dst_ptr;
          result.whole2dst = result2.whole2dst;
          result.dst2src = result2.dst2src;
          // Found a downcast which can't be bettered or an ambiguous downcast
          // which can't be disambiguated
          return result2_ambig;
        }
      
      if (!result_ambig && !result.dst_ptr)
        {
          // Not found anything yet.
          result.dst_ptr = result2.dst_ptr;
          result.whole2dst = result2.whole2dst;
          result_ambig = result2_ambig;
        }
      else if (result.dst_ptr && result.dst_ptr == result2.dst_ptr)
        {
          // Found at same address, must be via virtual.  Pick the most
          // accessible path.
          result.whole2dst =
              __sub_kind (result.whole2dst | result2.whole2dst);
        }
      else if ((result.dst_ptr && result2.dst_ptr)
               || (result_ambig && result2.dst_ptr)
               || (result2_ambig && result.dst_ptr))
        {
          // Found two different DST_TYPE bases, or a valid one and a set of
          // ambiguous ones, must disambiguate. See whether SRC_PTR is
          // contained publicly within one of the non-ambiguous choices. If it
          // is in only one, then that's the choice. If it is in both, then
          // we're ambiguous and fail. If it is in neither, we're ambiguous,
          // but don't yet fail as we might later find a third base which does
          // contain SRC_PTR.
        
          __sub_kind new_sub_kind = result2.dst2src;
          __sub_kind old_sub_kind = result.dst2src;
          
          if (contained_nonvirtual_p (result.whole2src))
            {
              // We already found SRC_PTR as a non-virtual base of most
              // derived. Therefore if it is in either choice, it can only be
              // in one of them, and we will already know.
              if (old_sub_kind == __unknown)
                old_sub_kind = __not_contained;
              if (new_sub_kind == __unknown)
                new_sub_kind = __not_contained;
            }
          else
            {
              if (old_sub_kind >= __not_contained)
                ;// already calculated
              else if (contained_nonvirtual_p (new_sub_kind))
                // Already found non-virtually inside the other choice,
                // cannot be in this.
                old_sub_kind = __not_contained;
              else
                old_sub_kind = dst_type->__find_public_src
                                (src2dst, result.dst_ptr, src_type, src_ptr);
          
              if (new_sub_kind >= __not_contained)
                ;// already calculated
              else if (contained_nonvirtual_p (old_sub_kind))
                // Already found non-virtually inside the other choice,
                // cannot be in this.
                new_sub_kind = __not_contained;
              else
                new_sub_kind = dst_type->__find_public_src
                                (src2dst, result2.dst_ptr, src_type, src_ptr);
            }
          
          // Neither sub_kind can be contained_ambig -- we bail out early
          // when we find those.
          if (contained_p (__sub_kind (new_sub_kind ^ old_sub_kind)))
            {
              // Only on one choice, not ambiguous.
              if (contained_p (new_sub_kind))
                {
                  // Only in new.
                  result.dst_ptr = result2.dst_ptr;
                  result.whole2dst = result2.whole2dst;
                  result_ambig = false;
                  old_sub_kind = new_sub_kind;
                }
              result.dst2src = old_sub_kind;
              if (public_p (result.dst2src))
                return false; // Can't be an ambiguating downcast for later discovery.
              if (!virtual_p (result.dst2src))
                return false; // Found non-virtually can't be bettered
            }
          else if (contained_p (__sub_kind (new_sub_kind & old_sub_kind)))
            {
              // In both.
              result.dst_ptr = NULL;
              result.dst2src = __contained_ambig;
              return true;  // Fail.
            }
          else
            {
              // In neither publicly, ambiguous for the moment, but keep
              // looking. It is possible that it was private in one or
              // both and therefore we should fail, but that's just tough.
              result.dst_ptr = NULL;
              result.dst2src = __not_contained;
              result_ambig = true;
            }
        }
      
      if (result.whole2src == __contained_private)
        // We found SRC_PTR as a private non-virtual base, therefore all
        // cross casts will fail. We have already found a down cast, if
        // there is one.
        return result_ambig;
    }

  return result_ambig;
}

bool __class_type_info::
__do_upcast (__sub_kind access_path,
             const __class_type_info *dst, const void *obj,
             __upcast_result &__restrict result) const
{
  if (*this == *dst)
    {
      result.dst_ptr = obj;
      result.base_type = nonvirtual_base_type;
      result.whole2dst = access_path;
      return contained_nonpublic_p (access_path);
    }
  return false;
}

bool __si_class_type_info::
__do_upcast (__sub_kind access_path,
             const __class_type_info *dst, const void *obj_ptr,
             __upcast_result &__restrict result) const
{
  if (*this == *dst)
    {
      result.dst_ptr = obj_ptr;
      result.base_type = nonvirtual_base_type;
      result.whole2dst = access_path;
      return contained_nonpublic_p (access_path);
    }
  return base->__do_upcast (access_path, dst, obj_ptr, result);
}

bool __vmi_class_type_info::
__do_upcast (__sub_kind access_path,
             const __class_type_info *dst, const void *obj_ptr,
             __upcast_result &__restrict result) const
{
  if (*this == *dst)
    {
      result.dst_ptr = obj_ptr;
      result.base_type = nonvirtual_base_type;
      result.whole2dst = access_path;
      return contained_nonpublic_p (access_path);
    }
  
  int src_details = result.src_details;
  if (src_details & __flags_unknown_mask)
    src_details = vmi_flags;
  
  for (size_t i = vmi_base_count; i--;)
    {
      __upcast_result result2 (src_details);
      const void *base = obj_ptr;
      __sub_kind sub_access = access_path;
      ptrdiff_t offset = vmi_bases[i].__offset ();
      bool is_virtual = vmi_bases[i].__is_virtual_p ();
      
      if (!vmi_bases[i].__is_public_p ())
        {
          if (!(src_details & non_diamond_repeat_mask))
            // original cannot have an ambiguous base
            continue;
          sub_access = __sub_kind (sub_access & ~__contained_public_mask);
        }
      if (is_virtual)
    	  sub_access = __sub_kind (sub_access | __contained_virtual_mask);
      if (base)
        base = convert_to_base (base, is_virtual, offset);
      
      if (vmi_bases[i].base->__do_upcast (sub_access, dst, base, result2))
        return true; // must fail
      if (result2.base_type)
        {
          if (result2.base_type == nonvirtual_base_type && is_virtual)
            result2.base_type = vmi_bases[i].base;
          if (!result.base_type)
            {
              result = result2;
              if (!(vmi_flags & non_diamond_repeat_mask))
                // cannot have an ambiguous other base
                return false;
            }
          else if (result.dst_ptr != result2.dst_ptr)
            {
              // Found an ambiguity.
	      result.dst_ptr = NULL;
	      result.whole2dst = __contained_ambig;
	      return true;
            }
          else if (result.dst_ptr)
            {
              // Ok, found real object via a virtual path.
              result.whole2dst
                  = __sub_kind (result.whole2dst | result2.whole2dst);
            }
          else
            {
              // Dealing with a null pointer, need to check vbase
              // containing each of the two choices.
              if (result2.base_type == nonvirtual_base_type
                  || result.base_type == nonvirtual_base_type
                  || !(*result2.base_type == *result.base_type))
                {
                  // Already ambiguous, not virtual or via different virtuals.
                  // Cannot match.
                  result.whole2dst = __contained_ambig;
                  return true;
                }
            }
        }
    }
  return false;
}

// this is the external interface to the dynamic cast machinery
void *
__dynamic_cast (const void *src_ptr,    // object started from
                const __class_type_info *src_type, // type of the starting object
                const __class_type_info *dst_type, // desired target type
                ptrdiff_t src2dst) // how src and dst are related
{
  const void *vtable = *static_cast <const void *const *> (src_ptr);
  const vtable_prefix *prefix =
      adjust_pointer <vtable_prefix> (vtable, 
				      -offsetof (vtable_prefix, origin));
  const void *whole_ptr =
      adjust_pointer <void> (src_ptr, prefix->whole_object);
  const __class_type_info *whole_type = prefix->whole_type;
  __class_type_info::__dyncast_result result;
  
  whole_type->__do_dyncast (src2dst, __class_type_info::__contained_public,
                            dst_type, whole_ptr, src_type, src_ptr, result);
  if (!result.dst_ptr)
    return NULL;
  if (contained_public_p (result.dst2src))
    return const_cast <void *> (result.dst_ptr);
  if (contained_public_p (__class_type_info::__sub_kind (result.whole2src & result.whole2dst)))
    // Found a valid cross cast
    return const_cast <void *> (result.dst_ptr);
  if (contained_nonvirtual_p (result.whole2src))
    // Found an invalid cross cast, which cannot also be a down cast
    return NULL;
  #if 0 // FIXME: we need to discover this lazily
  if (!(whole_type->details & __class_type_info::private_base_mask))
    // whole type has no private bases
    return const_cast <void *> (result.dst_ptr);
  #endif
  if (result.dst2src == __class_type_info::__unknown)
    result.dst2src = dst_type->__find_public_src (src2dst, result.dst_ptr,
                                                  src_type, src_ptr);
  if (contained_public_p (result.dst2src))
    // Found a valid down cast
    return const_cast <void *> (result.dst_ptr);
  // Must be an invalid down cast, or the cross cast wasn't bettered
  return NULL;
}

}; // namespace __cxxabiv1
#endif
