/* Implementation of Fortran type abstraction
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "proj.h"
#include "type.h"
#include "malloc.h"


/* Look up a type given its base type and kind value.  */

ffetype
ffetype_lookup_kind (ffetype base_type, int kind)
{
  if ((base_type->kinds_ == NULL)
      || (kind < 0)
      || (((size_t) kind) >= ARRAY_SIZE (base_type->kinds_->type_)))
    return NULL;

  return base_type->kinds_->type_[kind];
}

ffetype
ffetype_lookup_star (ffetype base_type, int star)
{
  if ((base_type->stars_ == NULL)
      || (star < 0)
      || (((size_t) star) >= ARRAY_SIZE (base_type->stars_->type_)))
    return NULL;

  return base_type->stars_->type_[star];
}

ffetype
ffetype_new (void)
{
  ffetype type;

  type = malloc_new_kp (malloc_pool_image (), "ffetype", sizeof (*type));
  type->kinds_ = NULL;
  type->stars_ = NULL;
  type->alignment_ = 0;
  type->modulo_ = 0;
  type->size_ = 0;

  return type;
}

void
ffetype_set_kind (ffetype base_type, int kind, ffetype type)
{
  assert (kind < (int) sizeof (*(base_type->kinds_)));

  if (base_type->kinds_ == NULL)
    {
      int i;

      base_type->kinds_
	= malloc_new_kp (malloc_pool_image (), "ffetype_indexes_[kinds]",
			 sizeof (*(base_type->kinds_)));
      for (i = 0; ((size_t) i) < ARRAY_SIZE (base_type->kinds_->type_); ++i)
	base_type->kinds_->type_[i] = NULL;
    }

  assert (base_type->kinds_->type_[kind] == NULL);

  base_type->kinds_->type_[kind] = type;
}

void
ffetype_set_star (ffetype base_type, int star, ffetype type)
{
  if (base_type->stars_ == NULL)
    {
      int i;

      base_type->stars_
	= malloc_new_kp (malloc_pool_image (), "ffetype_indexes_[stars]",
			 sizeof (*(base_type->stars_)));
      for (i = 0; ((size_t) i) < ARRAY_SIZE (base_type->stars_->type_); ++i)
	base_type->stars_->type_[i] = NULL;
    }

  assert (base_type->stars_->type_[star] == NULL);

  base_type->stars_->type_[star] = type;
}
