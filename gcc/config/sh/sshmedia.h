/* Copyright (C) 2000-2014 Free Software Foundation, Inc.

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


/* sshmedia.h: Intrinsics corresponding to SHmedia instructions that
   may only be executed in privileged mode.  */

#ifndef _SSHMEDIA_H
#define _SSHMEDIA_H

#if __SHMEDIA__
__inline__ static unsigned long long sh_media_GETCON (unsigned int k)
  __attribute__((always_inline));

__inline__ static
unsigned long long
sh_media_GETCON (unsigned int k)
{
  unsigned long long res;
  __asm__ __volatile__ ("getcon	cr%1, %0" : "=r" (res) : "n" (k));
  return res;
}

__inline__ static void sh_media_PUTCON (unsigned long long mm, unsigned int k)
  __attribute__((always_inline));

__inline__ static
void
sh_media_PUTCON (unsigned long long mm, unsigned int k)
{
  __asm__ __volatile__ ("putcon	%0, cr%1" : : "r" (mm), "n" (k));
}

__inline__ static
unsigned long long
sh_media_GETCFG (unsigned long long mm, int s)
{
  unsigned long long res;
  __asm__ __volatile__ ("getcfg	%1, %2, %0" : "=r" (res) : "r" (mm), "n" (s));
  return res;
}

__inline__ static
void
sh_media_PUTCFG (unsigned long long mm, int s, unsigned long long mw)
{
  __asm__ __volatile__ ("putcfg	%0, %1, %2" : : "r" (mm), "n" (s), "r" (mw));
}

__inline__ static
void
sh_media_SLEEP (void)
{
  __asm__ __volatile__ ("sleep");
}
#endif

#endif
