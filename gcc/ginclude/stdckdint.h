/* Copyright (C) 2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* ISO C23: 7.20 Checked Integer Arithmetic <stdckdint.h>.  */

#ifndef _STDCKDINT_H
#define _STDCKDINT_H

#define __STDC_VERSION_STDCKDINT_H__ 202311L

#define ckd_add(r, a, b) ((_Bool) __builtin_add_overflow (a, b, r))
#define ckd_sub(r, a, b) ((_Bool) __builtin_sub_overflow (a, b, r))
#define ckd_mul(r, a, b) ((_Bool) __builtin_mul_overflow (a, b, r))

/* Allow for the C library to add its part to the header.  */
#if !defined (_LIBC_STDCKDINT_H) && __has_include_next (<stdckdint.h>)
# include_next <stdckdint.h>
#endif

#endif /* stdckdint.h */
