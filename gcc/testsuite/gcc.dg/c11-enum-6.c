/* Test C2x enumerations with fixed underlying type are not diagnosed for C11
   with -pedantic-errors -Wno-c11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-c11-c2x-compat" } */

enum e1 : int;
enum e2 : short { A };
enum : short { B };
