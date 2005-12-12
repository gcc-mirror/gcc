/* Copyright (C) 2004 Free Software Foundation

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301 USA.  */

struct generate_random_data
  {
    int *fptr, *rptr, *state;
    int rand_type, rand_deg, rand_sep;
    int *end_ptr;
  };

extern void generate_srandom (unsigned int);
extern char *generate_initstate (unsigned int, char *, size_t);
extern char *generate_setstate (char *);
extern long int generate_random (void);
extern int generate_random_r (struct generate_random_data *, int *);
extern int generate_srandom_r (unsigned int, struct generate_random_data *);
extern int generate_initstate_r (unsigned int, char *, size_t,
				 struct generate_random_data *);
extern int generate_setstate_r (char *, struct generate_random_data *);
