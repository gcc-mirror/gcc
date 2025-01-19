(* Builtins.mod provides access to all built-in functions.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE Builtins ;

IMPORT cbuiltin, wrapc ;


PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_alloca)) alloca (i: CARDINAL) : ADDRESS ;
BEGIN
   (* This routine will never be called as it allocates memory on
      top of the current stack frame, which is automatically
      deallocated upon its return.  *)
   HALT ;
   RETURN NIL
END alloca ;

PROCEDURE alloca_trace (returned: ADDRESS; nBytes: CARDINAL) : ADDRESS ;
BEGIN
   (* this routine is only called if -fdebug-builtins is supplied
      on the command line.  The purpose of this routine is to allow
      a developer to single step into this routine and inspect the
      value of nBytes and returned.   *)
   RETURN returned
END alloca_trace ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_memcpy))
          memcpy (dest, src: ADDRESS; nbytes: CARDINAL) : ADDRESS ;
BEGIN
   (* Hopefully the compiler will choose to use the __builtin_memcpy
      function within GCC.  This call is here just in case it cannot.
      If the user sets a procedure variable to memcpy then the
      code below could be run instead.   *)
   RETURN cbuiltin.memcpy (dest, src, nbytes)
END memcpy ;


PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isnan)) isnanf (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN wrapc.isnanf (x)
END isnanf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isnan)) isnan (x: REAL) : INTEGER ;
BEGIN
   RETURN wrapc.isnan (x)
END isnan ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isnan)) isnanl (x: LONGREAL) : INTEGER ;
BEGIN
   RETURN wrapc.isnanl (x)
END isnanl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_isfinite)) isfinitef (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN wrapc.isfinitef (x)
END isfinitef ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_isfinite)) isfinite (x: REAL) : INTEGER ;
BEGIN
   RETURN wrapc.isfinite (x)
END isfinite ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_isfinite)) isfinitel (x: LONGREAL) : INTEGER ;
BEGIN
   RETURN wrapc.isfinitel (x)
END isfinitel ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sin)) sin (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.sin (x)
END sin ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinf)) sinf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sinf (x)
END sinf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinl)) sinl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.sinl (x)
END sinl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cos)) cos (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.cos (x)
END cos ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosf)) cosf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cosf (x)
END cosf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosl)) cosl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cosl (x)
END cosl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_atan2)) atan2 (x, y: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.atan2 (x, y)
END atan2 ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_atan2f)) atan2f (x, y: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.atan2f (x, y)
END atan2f ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_atan2l)) atan2l (x, y: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.atan2l (x, y)
END atan2l ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sqrt)) sqrt (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.sqrt (x)
END sqrt ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sqrtf)) sqrtf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sqrtf (x)
END sqrtf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sqrtl)) sqrtl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.sqrtl (x)
END sqrtl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_fabs)) fabs (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.fabs (x)
END fabs ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_fabsf)) fabsf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.fabsf (x)
END fabsf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_fabsl)) fabsl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.fabsl (x)
END fabsl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log)) log (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.log (x)
END log ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_logf)) logf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.logf (x)
END logf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_logl)) logl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.logl (x)
END logl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp)) exp (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.exp (x)
END exp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_expf)) expf (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.expf (x)
END expf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_expl)) expl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.expl (x)
END expl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log10)) log10 (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.log10 (x)
END log10 ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log10f)) log10f (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.log10f (x)
END log10f ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_log10l)) log10l (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.log10l (x)
END log10l ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp10)) exp10 (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.exp10 (x)
END exp10 ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp10f)) exp10f (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.exp10f (x)
END exp10f ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_exp10l)) exp10l (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.exp10l (x)
END exp10l ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_ilogb)) ilogb (x: REAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.ilogb (x)
END ilogb ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_ilogbf)) ilogbf (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.ilogbf (x)
END ilogbf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_ilogbl)) ilogbl (x: LONGREAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.ilogbl (x)
END ilogbl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_modf)) modf (x: REAL; VAR y: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.modf (x, y)
END modf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_modff)) modff (x: SHORTREAL; VAR y: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.modff (x, y)
END modff ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_modfl)) modfl (x: LONGREAL; VAR y: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.modfl (x, y)
END modfl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_signbit)) signbit (r: REAL) : INTEGER ;
BEGIN
   RETURN wrapc.signbit (r)
END signbit ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_signbitf)) signbitf (s: SHORTREAL) : INTEGER ;
BEGIN
   RETURN wrapc.signbitf (s)
END signbitf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_signbitl)) signbitl (l: LONGREAL) : INTEGER ;
BEGIN
   RETURN wrapc.signbitl (l)
END signbitl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_nextafter)) nextafter (x, y: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.nextafter (x, y)
END nextafter ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_nextafterf)) nextafterf (x, y: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.nextafterf (x, y)
END nextafterf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_nextafterl)) nextafterl (x, y: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.nextafterl (x, y)
END nextafterl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_nexttoward)) nexttoward (x: REAL; y: LONGREAL) : REAL ;
BEGIN
   RETURN cbuiltin.nexttoward (x, y)
END nexttoward ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_nexttowardf)) nexttowardf (x: SHORTREAL; y: LONGREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.nexttowardf (x, y)
END nexttowardf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_nexttowardl)) nexttowardl (x, y: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.nexttowardl (x, y)
END nexttowardl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_scalbln)) scalbln (x: REAL; n: LONGINT) : REAL ;
BEGIN
   RETURN cbuiltin.scalbln (x, n)
END scalbln ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_scalblnf)) scalblnf (x: SHORTREAL; n: LONGINT) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.scalblnf (x, n)
END scalblnf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_scalblnl)) scalblnl (x: LONGREAL; n: LONGINT) : LONGREAL ;
BEGIN
   RETURN cbuiltin.scalblnl (x, n)
END scalblnl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_scalbn)) scalbn (x: REAL; n: INTEGER) : REAL ;
BEGIN
   RETURN cbuiltin.scalbn (x, n)
END scalbn ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_scalbnf)) scalbnf (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.scalbnf (x, n)
END scalbnf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_scalbnl)) scalbnl (x: LONGREAL; n: INTEGER) : LONGREAL ;
BEGIN
   RETURN cbuiltin.scalbnl (x, n)
END scalbnl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cabsf)) cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cabsf(z)
END cabsf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cabs)) cabs (z: COMPLEX) : REAL ;
BEGIN
   RETURN cbuiltin.cabs(z)
END cabs ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cabsl)) cabsl (z: LONGCOMPLEX) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cabsl(z)
END cabsl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cargf)) cargf (z: SHORTCOMPLEX) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cargf(z)
END cargf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_carg)) carg (z: COMPLEX) : REAL ;
BEGIN
   RETURN cbuiltin.carg(z)
END carg ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cargl)) cargl (z: LONGCOMPLEX) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cargl(z)
END cargl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_conjf)) conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.conjf(z)
END conjf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_conj)) conj (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.conj(z)
END conj ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_conjl)) conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.conjl(z)
END conjl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpowf)) cpowerf (base: SHORTCOMPLEX; exp: SHORTREAL) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.cpowf(base, exp)
END cpowerf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpow)) cpower (base: COMPLEX; exp: REAL) : COMPLEX ;
BEGIN
   RETURN cbuiltin.cpow(base, exp)
END cpower ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpowl)) cpowerl (base: LONGCOMPLEX; exp: LONGREAL) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.cpowl(base, exp)
END cpowerl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrtf)) csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.csqrtf(z)
END csqrtf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrt)) csqrt (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.csqrt(z)
END csqrt ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrtl)) csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.csqrtl(z)
END csqrtl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexpf)) cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.cexpf(z)
END cexpf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexp)) cexp (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.cexp(z)
END cexp ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexpl)) cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.cexpl(z)
END cexpl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_clogf)) clnf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.clogf(z)
END clnf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_clog)) cln (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.clog(z)
END cln ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_clogl)) clnl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.clogl(z)
END clnl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csinf)) csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.csinf(z)
END csinf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csin)) csin (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.csin(z)
END csin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csinl)) csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.csinl(z)
END csinl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccosf)) ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.ccosf(z)
END ccosf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccos)) ccos (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.ccos(z)
END ccos ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccosl)) ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.ccosl(z)
END ccosl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctanf)) ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.ctanf(z)
END ctanf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctan)) ctan (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.ctan(z)
END ctan ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctanl)) ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.ctanl(z)
END ctanl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_casinf)) carcsinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.casinf(z)
END carcsinf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_casin)) carcsin (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.casin(z)
END carcsin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_casinl)) carcsinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.casinl(z)
END carcsinl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cacosf)) carccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.cacosf(z)
END carccosf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cacos)) carccos (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.cacos(z)
END carccos ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cacosl)) carccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.cacosl(z)
END carccosl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_catanf)) carctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
BEGIN
   RETURN cbuiltin.catanf(z)
END carctanf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_catan)) carctan (z: COMPLEX) : COMPLEX ;
BEGIN
   RETURN cbuiltin.catan(z)
END carctan ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_catanl)) carctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
BEGIN
   RETURN cbuiltin.catanl(z)
END carctanl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_index)) index (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.index (s, c)
END index ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_rindex)) rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.rindex (s, c)
END rindex ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_memcmp)) memcmp (s1, s2: ADDRESS; nbytes: CARDINAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.memcmp (s1, s2, nbytes)
END memcmp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_memset)) memset (s: ADDRESS; c: INTEGER; nbytes: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.memset (s, c, nbytes)
END memset ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_memmove)) memmove (s1, s2: ADDRESS; nbytes: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.memmove (s1, s2, nbytes)
END memmove ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcat)) strcat (dest, src: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strcat (dest, src)
END strcat ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strncat)) strncat (dest, src: ADDRESS; nbytes: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strncat (dest, src, nbytes)
END strncat ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcpy)) strcpy (dest, src: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strcpy (dest, src)
END strcpy ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strncpy)) strncpy (dest, src: ADDRESS; nbytes: CARDINAL) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strncpy (dest, src, nbytes)
END strncpy ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcmp)) strcmp (s1, s2: ADDRESS) : INTEGER ;
BEGIN
   RETURN cbuiltin.strcmp (s1, s2)
END strcmp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strncmp)) strncmp (s1, s2: ADDRESS; nbytes: CARDINAL) : INTEGER ;
BEGIN
   RETURN cbuiltin.strncmp (s1, s2, nbytes)
END strncmp ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strlen)) strlen (s: ADDRESS) : INTEGER ;
BEGIN
   RETURN cbuiltin.strlen (s)
END strlen ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strstr)) strstr (haystack, needle: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strstr (haystack, needle)
END strstr ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strpbrk)) strpbrk (s, accept: ADDRESS) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strpbrk (s, accept)
END strpbrk ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strspn)) strspn (s, accept: ADDRESS) : CARDINAL ;
BEGIN
   RETURN cbuiltin.strspn (s, accept)
END strspn ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strcspn)) strcspn (s, accept: ADDRESS) : CARDINAL ;
BEGIN
   RETURN cbuiltin.strcspn (s, accept)
END strcspn ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strchr)) strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strchr (s, c)
END strchr ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_strrchr)) strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
BEGIN
   RETURN cbuiltin.strrchr (s, c)
END strrchr ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_val)) huge_val () : REAL ;
BEGIN
   RETURN -1.0
END huge_val ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_vall)) huge_vall () : LONGREAL ;
BEGIN
   RETURN -1.0
END huge_vall ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_huge_valf)) huge_valf () : SHORTREAL ;
BEGIN
   RETURN -1.0
END huge_valf ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_isgreater)) isgreater (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END isgreater ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isgreaterf)) isgreaterf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END isgreaterf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isgreaterl)) isgreaterl (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END isgreaterl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isgreaterequal)) isgreaterequal (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END isgreaterequal ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isgreaterequalf)) isgreaterequalf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END isgreaterequalf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isgreaterequall)) isgreaterequall (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END isgreaterequall ;


PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isless)) isless (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END isless ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessf)) islessf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END islessf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessl)) islessl (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END islessl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessequal)) islessequal (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END islessequal ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessequalf)) islessequalf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END islessequalf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessequall)) islessequall (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END islessequall ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessgreater)) islessgreater (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END islessgreater ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessgreaterf)) islessgreaterf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END islessgreaterf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_islessgreaterl)) islessgreaterl (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END islessgreaterl ;


PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isunordered)) isunordered (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END isunordered ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isunorderedf)) isunorderedf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END isunorderedf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isunorderedl)) isunorderedl (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END isunorderedl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_iseqsig)) iseqsig (x, y: REAL) : INTEGER ;
BEGIN
   RETURN 1
END iseqsig ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_iseqsigf)) iseqsigf (x, y: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END iseqsigf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_iseqsigl)) iseqsigl (x, y: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END iseqsigl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isnormal)) isnormal (r: REAL) : INTEGER ;
BEGIN
   RETURN 1
END isnormal ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isnormalf)) isnormalf (s: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END isnormalf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isnormall)) isnormall (l: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END isnormall ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isinf)) isinf_sign (r: REAL) : INTEGER ;
BEGIN
   RETURN 1
END isinf_sign ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isinf_signf)) isinf_signf (s: SHORTREAL) : INTEGER ;
BEGIN
   RETURN 1
END isinf_signf ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_isinf)) isinf_signl (l: LONGREAL) : INTEGER ;
BEGIN
   RETURN 1
END isinf_signl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_longjmp)) longjmp (env: ADDRESS; val: INTEGER) ;
BEGIN
   (* Empty, replaced internally by gcc.  *)
END longjmp ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_setjmp)) setjmp (env: ADDRESS) : INTEGER ;
BEGIN
   (* Empty, replaced internally by gcc.  *)
   RETURN 0   (* Keep -Wreturn-type happy.  *)
END setjmp ;

(*
   frame_address - returns the address of the frame.
                   The current frame is obtained if level is 0,
                   the next level up is level is 1 etc.
*)

PROCEDURE __ATTRIBUTE__ __BUILTIN__
         ((__builtin_frame_address))
         frame_address (level: CARDINAL) : ADDRESS ;
BEGIN
   RETURN NIL
END frame_address ;


(*
   return_address - returns the return address of function.
                    The current function return address is
                    obtained if level is 0,
                    the next level up is level is 1 etc.
*)

PROCEDURE __ATTRIBUTE__ __BUILTIN__
         ((__builtin_return_address))
         return_address (level: CARDINAL) : ADDRESS ;
BEGIN
   RETURN NIL
END return_address ;


END Builtins.
