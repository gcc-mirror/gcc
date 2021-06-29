/* CTF does not have representation for some types at this time.  These types
   are skipped in the CTF generation phase in the compiler.

   Skip Decimal Floating Point format types for CTF generation.

   In this testcase, CTF records for types are not added as CTF has no
   representation for Decimal floating point format.

   CTF records for variables do exist, however.  The referenced type is
   CTF_TYPE_NULLID.  */

/* { dg-do compile } */
/* { dg-options "-gctf" } */
/* { dg-require-effective-target dfp } */

_Decimal32 d32;
_Decimal64 d64;
_Decimal128 d128;
