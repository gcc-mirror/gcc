// Methods for type_info for -*- C++ -*- Run Time Type Identification.
// Copyright (C) 1994, 1996, 1998, 1999 Free Software Foundation

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

// dynamic_cast helper methods.
// Returns 1 if the cast succeeds, 0 otherwise.  Stores the adjusted value
// in VALP.

int __user_type_info::
dcast (const type_info& to, int, void *addr, void **valp,
       const type_info *, void *) const
{
  *valp = addr;
  return (*this == to);
}

int __si_type_info::
dcast (const type_info& to, int require_public, void *addr, void **valp,
       const type_info *sub, void *subptr) const
{
  if (*this == to)
    {
      *valp = addr;
      return 1;
    }
  return base.dcast (to, require_public, addr, valp, sub, subptr);
}

int __class_type_info::
dcast (const type_info& desired, int is_public, void *objptr, void **valp,
       const type_info *sub, void *subptr) const
{
  *valp = objptr;

  if (*this == desired)
    return 1;

  int match_found = 0;
  void *match = 0;

  for (size_t i = 0; i < n_bases; i++)
    {
      if (is_public && base_list[i].access != PUBLIC)
	continue;

      void *p;

      if (objptr)
	{
	  p = (char *)objptr + base_list[i].offset;
	  if (base_list[i].is_virtual)
	    p = *(void **)p;
	}
      else
	/* Preserve null pointer.  */
	p = objptr;

      if (base_list[i].base->dcast (desired, is_public, p, &p, sub, subptr))
	{
	  if (! match_found)
	    {
	      match_found = 1;
	      match = p;
	    }
	  else if (match != p)
	    {
	      if (sub)
		{
		  // Perhaps we're downcasting from *sub to desired; see if
		  // subptr is a subobject of exactly one of {match_found,p}.

		  const __user_type_info &d =
		    static_cast <const __user_type_info &> (desired);

		  void *os;
		  d.dcast (*sub, 1, match, &os);
		  void *ns;
		  d.dcast (*sub, 1, p, &ns);

		  if (os == ns)
		    // Both have the same subobject, so we can't disambiguate;
		    // i.e. subptr is a virtual base.
		    return 0;
		  else if (os == subptr)
		    continue;
		  else if (ns == subptr)
		    {
		      match = p;
		      continue;
		    }
		}
	      else
		// We're not downcasting, so we can't disambiguate.
		return 0;
	    }
	}
    }

  *valp = match;
  return match_found;
}
