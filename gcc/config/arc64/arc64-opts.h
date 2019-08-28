/* Copyright (C) 2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef ARC64_OPTS_H
#define ARC64_OPTS_H

/* The code model defines the address generation strategy.  */
enum arc64_code_model {
  /* Static code and data fit within a 1MB region.
     The default non-PIC code model.  */
  ARC64_CMODEL_SMALL,
  /* The default for PIC code model, static code and data fit within
     4GB region.  Local calls will fit within 16MB region.  */
  ARC64_CMODEL_MEDIUM,
  /* No assumptions about addresses of code and data.  */
  ARC64_CMODEL_LARGE
};

#endif /* ARC64_OPTS_H */
