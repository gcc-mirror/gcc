// Methods for type_info for -*- C++ -*- Run Time Type Identification.
// Copyright (C) 1994, 1996, 1997, 1998, 1999, 2000 Free Software Foundation

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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <stddef.h>
#include "tinfo.h"
#include "new"			// for placement new

using std::type_info;

#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
bool
type_info::before (const type_info &arg) const
{
  return __builtin_strcmp (name (), arg.name ()) < 0;
}

// type info for pointer type.

struct __pointer_type_info : public type_info {
  const type_info& type;

  __pointer_type_info (const char *n, const type_info& ti)
    : type_info (n), type (ti) {}
};

// type info for attributes

struct __attr_type_info : public type_info {
  enum cv { NONE = 0, CONST = 1, VOLATILE = 2, CONSTVOL = 1 | 2 };

  const type_info& type;
  cv attr;

  __attr_type_info (const char *n, cv a, const type_info& t)
    : type_info (n), type (t), attr (a) {}
};

// type_info for builtin type

struct __builtin_type_info : public type_info {
  __builtin_type_info (const char *n): type_info (n) {}
};

// type info for function.

struct __func_type_info : public type_info {
  __func_type_info (const char *n) : type_info (n) {}
};

// type info for pointer to member function.

struct __ptmf_type_info : public type_info {
  __ptmf_type_info (const char *n) : type_info (n) {}
};

// type info for pointer to data member.

struct __ptmd_type_info : public type_info {
  __ptmd_type_info (const char *n): type_info (n) {}
};

// type info for array.

struct __array_type_info : public type_info {
  __array_type_info (const char *n): type_info (n) {}
};

#else

#include <cxxabi.h>
#endif

#if defined(__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
namespace __cxxabiv1 {

using namespace std;

// This has special meaning to the compiler, and will cause it
// to emit the type_info structures for the fundamental types which are
// mandated to exist in the runtime.
__fundamental_type_info::
~__fundamental_type_info ()
{}

__pointer_type_info::
~__pointer_type_info ()
{}

__array_type_info::
~__array_type_info ()
{}

__function_type_info::
~__function_type_info ()
{}

__enum_type_info::
~__enum_type_info ()
{}

__pointer_to_member_type_info::
~__pointer_to_member_type_info ()
{}

bool __pointer_type_info::
__is_pointer_p () const
{
  return true;
}

bool __function_type_info::
__is_function_p () const
{
  return true;
}

bool __pointer_to_member_type_info::
__is_pointer_p () const
{
  return false;
}


bool __pointer_type_info::
__do_catch (const type_info *thr_type,
            void **thr_obj,
            unsigned outer) const
{
  if (*this == *thr_type)
    return true;      // same type
  if (typeid (*this) != typeid (*thr_type))
    return false;     // not both same kind of pointers
  
  if (!(outer & 1))
    // We're not the same and our outer pointers are not all const qualified
    // Therefore there must at least be a qualification conversion involved
    // But for that to be valid, our outer pointers must be const qualified.
    return false;
  
  const __pointer_type_info *thrown_type =
    static_cast <const __pointer_type_info *> (thr_type);
  
  if (thrown_type->quals & ~quals)
    // We're less qualified.
    return false;
  
  if (!(quals & const_mask))
    outer &= ~1;
  
  return __pointer_catch (thrown_type, thr_obj, outer);
}

bool __pointer_type_info::
__pointer_catch (const __pointer_type_info *thrown_type,
                 void **thr_obj,
                 unsigned outer) const
{
  if (outer < 2 && *type == typeid (void))
    {
      // conversion to void
      return !thrown_type->type->__is_function_p ();
    }
  
  return type->__do_catch (thrown_type->type, thr_obj, outer + 2);
}

bool __pointer_to_member_type_info::
__pointer_catch (const __pointer_type_info *thr_type,
                 void **thr_obj,
                 unsigned outer) const
{
  // This static cast is always valid, as our caller will have determined that
  // thr_type is really a __pointer_to_member_type_info.
  const __pointer_to_member_type_info *thrown_type =
    static_cast <const __pointer_to_member_type_info *> (thr_type);
  
  if (*klass != *thrown_type->klass)
    return false;     // not pointers to member of same class
  
  return type->__do_catch (thrown_type->type, thr_obj, outer + 2);
}

} // namespace std
#endif

// Entry points for the compiler.

/* Low level match routine used by compiler to match types of catch
   variables and thrown objects.  */

extern "C" int
__throw_type_match_rtti_2 (const void *catch_type_r, const void *throw_type_r,
			 void *objptr, void **valp)
{
  const type_info &catch_type = *(const type_info *)catch_type_r;
  const type_info &throw_type = *(const type_info *)throw_type_r;

  *valp = objptr;

#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
// old abi
  if (catch_type == throw_type)
    return 1;
  
  if (const __user_type_info *p
      = dynamic_cast <const __user_type_info *> (&throw_type))
    {
      return p->upcast (catch_type, objptr, valp);
    }
  else if (const __pointer_type_info *fr =
	   dynamic_cast <const __pointer_type_info *> (&throw_type))
    {
      const __pointer_type_info *to =
	   dynamic_cast <const __pointer_type_info *> (&catch_type);

      if (! to)
	return 0;

      const type_info *subfr = &fr->type, *subto = &to->type;
      __attr_type_info::cv cvfrom, cvto;

      if (const __attr_type_info *at
	  = dynamic_cast <const __attr_type_info *> (subfr))
	{
	  cvfrom = at->attr;
	  subfr = &at->type;
	}
      else
	cvfrom = __attr_type_info::NONE;
      
      if (const __attr_type_info *at
	  = dynamic_cast <const __attr_type_info *> (subto))
	{
	  cvto = at->attr;
	  subto = &at->type;
	}
      else
	cvto = __attr_type_info::NONE;

      if (((cvfrom & __attr_type_info::CONST)
	   > (cvto & __attr_type_info::CONST))
	  || ((cvfrom & __attr_type_info::VOLATILE)
	      > (cvto & __attr_type_info::VOLATILE)))
	return 0;

      if (*subto == *subfr)
	return 1;
      else if (*subto == typeid (void)
	       && dynamic_cast <const __func_type_info *> (subfr) == 0)
	return 1;
      else if (const __user_type_info *p
	       = dynamic_cast <const __user_type_info *> (subfr))
	return p->upcast (*subto, objptr, valp);
      else if (const __pointer_type_info *pfr
	       = dynamic_cast <const __pointer_type_info *> (subfr))
	{
	  // Multi-level pointer conversion.

	  const __pointer_type_info *pto
	    = dynamic_cast <const __pointer_type_info *> (subto);

	  if (! pto)
	    return 0;
	    
	  bool constp = (cvto & __attr_type_info::CONST);
	  for (subto = &pto->type, subfr = &pfr->type; ;
	       subto = &pto->type, subfr = &pfr->type)
	    {
	      if (const __attr_type_info *at
		  = dynamic_cast <const __attr_type_info *> (subfr))
		{
		  cvfrom = at->attr;
		  subfr = &at->type;
		}
	      else
		cvfrom = __attr_type_info::NONE;
      
	      if (const __attr_type_info *at
		  = dynamic_cast <const __attr_type_info *> (subto))
		{
		  cvto = at->attr;
		  subto = &at->type;
		}
	      else
		cvto = __attr_type_info::NONE;

	      if (((cvfrom & __attr_type_info::CONST)
		   > (cvto & __attr_type_info::CONST))
		  || ((cvfrom & __attr_type_info::VOLATILE)
		      > (cvto & __attr_type_info::VOLATILE)))
		return 0;

	      if (! constp
		  && (((cvfrom & __attr_type_info::CONST)
		       < (cvto & __attr_type_info::CONST))
		      || ((cvfrom & __attr_type_info::VOLATILE)
			  < (cvto & __attr_type_info::VOLATILE))))
		return 0;

	      if (*subto == *subfr)
		return 1;

	      pto = dynamic_cast <const __pointer_type_info *> (subto);
	      pfr = dynamic_cast <const __pointer_type_info *> (subfr);
	      if (! pto || ! pfr)
		return 0;		

	      if (! (cvto & __attr_type_info::CONST))
		constp = false;
	    }
	}
    }
#else
// new abi
  
  return catch_type.__do_catch (&throw_type, valp, 1);
#endif
  return 0;
}

#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
/* Backward compatibility wrapper.  */

extern "C" void*
__throw_type_match_rtti (const void *catch_type_r, const void *throw_type_r,
			 void *objptr)
{
  void *ret;
  if (__throw_type_match_rtti_2 (catch_type_r, throw_type_r, objptr, &ret))
    return ret;
  return NULL;
}
#endif

/* Called from __cp_pop_exception.  Is P the type_info node for a pointer
   of some kind?  */

bool
__is_pointer (void *p)
{
  const type_info *t = reinterpret_cast <const type_info *>(p);
#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
// old abi
  const __pointer_type_info *pt =
    dynamic_cast <const __pointer_type_info *> (t);
  return pt != 0;
#else
// new abi
  return t->__is_pointer_p ();
#endif
}

#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
// old abi

extern "C" void
__rtti_ptr (void *addr, const char *n, const type_info *ti)
{ new (addr) __pointer_type_info (n, *ti); }

extern "C" void
__rtti_attr (void *addr, const char *n, int attrval, const type_info *ti)
{
  new (addr) __attr_type_info
    (n, static_cast <__attr_type_info::cv> (attrval), *ti);
}

extern "C" void
__rtti_func (void *addr, const char *name)
{ new (addr) __func_type_info (name); }

extern "C" void
__rtti_ptmf (void *addr, const char *name)
{ new (addr) __ptmf_type_info (name); }

extern "C" void
__rtti_ptmd (void *addr, const char *name)
{ new (addr) __ptmd_type_info (name); }

extern "C" void
__rtti_array (void *addr, const char *name)
{ new (addr) __array_type_info (name); }

extern "C" void *
__dynamic_cast (const type_info& (*from)(void), const type_info& (*to)(void),
		int require_public, void *address, const type_info & (*sub)(void), void *subptr)
{
  if (!require_public) abort();
  return static_cast <__user_type_info const &> (from ()).dyncast
      (/*boff=*/-1, to (), address, sub (), subptr);
}

extern "C" void *
__dynamic_cast_2 (const type_info& (*from)(void), const type_info& (*to)(void),
                  int boff,
		  void *address, const type_info & (*sub)(void), void *subptr)
{
  return static_cast <__user_type_info const &> (from ()).dyncast
      (boff, to (), address, sub (), subptr);
}

// type_info nodes and functions for the builtin types.  The mangling here
// must match the mangling in gcc/cp/rtti.c.

#define BUILTIN(mangled)					\
unsigned char __ti##mangled [sizeof (__builtin_type_info)]	\
  __attribute__ ((aligned (__alignof__ (void *))));		\
extern "C" const type_info &__tf##mangled (void) {		\
  if ((*(void **) __ti##mangled) == 0)				\
    new (__ti##mangled) __builtin_type_info (#mangled);		\
  return *(type_info *)__ti##mangled;				\
}

BUILTIN (v); BUILTIN (x); BUILTIN (l); BUILTIN (i); BUILTIN (s); BUILTIN (b);
BUILTIN (c); BUILTIN (w); BUILTIN (r); BUILTIN (d); BUILTIN (f);
BUILTIN (Ui); BUILTIN (Ul); BUILTIN (Ux); BUILTIN (Us); BUILTIN (Uc);
BUILTIN (Sc);

#endif
