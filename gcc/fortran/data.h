/* Header for functions resolving DATA statements.
   Copyright (C) 2007-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

void gfc_formalize_init_value (gfc_symbol *);
void gfc_get_section_index (gfc_array_ref *, mpz_t *, mpz_t *);
bool gfc_assign_data_value (gfc_expr *, gfc_expr *, mpz_t, mpz_t *);
void gfc_advance_section (mpz_t *, gfc_array_ref *, mpz_t *);
