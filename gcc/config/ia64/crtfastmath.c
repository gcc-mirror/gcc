/* Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by David Mosberger <davidm@hpl.hp.com>.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* In addition to the permissions in the GNU General Public License, the
   Free Software Foundation gives you unlimited permission to link the
   compiled version of this file into combinations with other programs,
   and to distribute those combinations without any restriction coming
   from the use of this file.  (The General Public License restrictions
   do apply in other respects; for example, they cover modification of
   the file, and distribution when not linked into a combine
   executable.)  */

/* We could call fesetenv() here but that would create a confusing
   dependency on libm (since that is where fesetenv() gets defined.
   To avoid this, just do everything locally.  */
#define FE_NONIEEE_ENV 0x0009a04d0270037f

static void __attribute__((constructor))
__ia64_set_fast_math (void)
{
  __asm__ __volatile__ ("mov.m ar.fpsr=%0" : : "r"(FE_NONIEEE_ENV));
}
