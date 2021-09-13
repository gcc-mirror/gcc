/* CTF does not have representation for some types at this time.  These types
   are skipped in the CTF generation phase in the compiler.

   Skip IEEE interchange and extended formats for CTF generation.

   In this testcase, CTF records for types are not added as CTF has no
   representation for IEEE interchange and extended formats.

   CTF records for variables and pointer do exist, however.  The referenced
   type is CTF_TYPE_NULLID.  */

/* { dg-do compile } */
/* { dg-options "-gctf" } */

/* { dg-require-effective-target float32 } */
/* { dg-require-effective-target float32x } */

_Float32 f32;
_Float32x f32x;
_Float32 * f32p;
