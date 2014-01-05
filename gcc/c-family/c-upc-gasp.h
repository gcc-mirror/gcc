/* c-upc-gasp.h: GASP instrumentation API.
   Copyright (C) 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_C_FAMILY_UPC_GASP_H
#define GCC_C_FAMILY_UPC_GASP_H 1

extern int disable_pupc_mode(void);
extern int get_upc_pupc_mode(void);
extern tree upc_gasp_add_src_args (tree, const char *, int);
extern tree upc_instrument_forall (location_t, int);
extern void set_pupc_mode(int);
extern void upc_instrument_func (tree);

#endif /* !GCC_C_FAMILY_UPC_GASP_H */
