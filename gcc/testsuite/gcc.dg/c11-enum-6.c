/* Test C23 enumerations with fixed underlying type are not diagnosed for C11
   with -pedantic-errors -Wno-c11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-c11-c23-compat" } */

enum e1 : int;
enum e2 : short { A };
enum : short { B };
