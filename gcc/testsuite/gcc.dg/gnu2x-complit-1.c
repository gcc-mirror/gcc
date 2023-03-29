/* Test C2x storage class specifiers in compound literals: GNU use of alignof
   on objects (tested separately since alignof parsing handles the type name of
   compound literals).  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x" } */

int a = alignof (static int) { 0 };
