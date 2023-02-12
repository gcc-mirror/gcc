(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE realbitscast;

FROM SYSTEM IMPORT CAST, WORD ;

#undef HAVE_REAL96

#if defined(__sparc__)
#   undef HAVE_REAL96
#elif defined(__alpha__) && defined(__arch64__)
#   define HAVE_REAL96
#elif defined(__ppc__)
#   undef HAVE_REAL96
#elif defined(__ia64)
#   undef HAVE_REAL96
#elif defined(__APPLE__)
(* No 96 bit floating type on Apple platforms *)
#   undef HAVE_REAL96
#endif


TYPE
    BITS32 = SET OF [0..31];
    BITS64 = SET OF [0..63];
    BITS96 = SET OF [0..95] ;
    REAL32 = SHORTREAL;
    REAL64 = REAL;

#if defined(HAVE_REAL96)
    REAL96 = LONGREAL ;
    (* and on the x86_64 LONGREAL is 128 bits *)
    (* this is also true for at least some alphas *)
#endif

VAR
    b32 : BITS32;
    b64 : BITS64;
    r32 : REAL32;
    r64 : REAL64;
#if defined(HAVE_REAL96)
    b96 : BITS96 ;
    r96 : REAL96 ;
#endif
    w   : WORD ;
BEGIN
   r32 := 1.0 ;
   b32 := CAST(BITS32, r32) ;
   b64 := CAST(BITS64, r64) ;
#if defined(HAVE_REAL96)
   b96 := CAST(BITS96, r96)
#endif
END realbitscast.
