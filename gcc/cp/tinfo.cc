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

namespace
{
// ADDR is a pointer to an object.  Convert it to a pointer to a base,
// using OFFSET.
inline void*
convert_to_base (void *addr, bool is_virtual, USItype offset)
{
  if (!addr)
    return NULL;

  if (!is_virtual)
    return (char *) addr + offset;

#if defined(__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
  // Under the new ABI, the offset gives us an index into the vtable,
  // which contains an offset to the virtual base.  The vptr is always
  // the first thing in the object.
  std::ptrdiff_t *vtable = *((std::ptrdiff_t **) addr);
  return ((char *) addr) + vtable[offset];
#else
  // Under the old ABI, the offset gives us the address of a pointer
  // to the virtual base.
  return *((void **) ((char *) addr + offset));
#endif
}

}

// This file contains the minimal working set necessary to link with code
// that uses virtual functions and -frtti but does not actually use RTTI
// functionality.

std::type_info::
~type_info ()
{ }

// We can't rely on common symbols being shared between shared objects.
bool std::type_info::
operator== (const std::type_info& arg) const
{
  return (&arg == this) || (strcmp (name (), arg.name ()) == 0);
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
