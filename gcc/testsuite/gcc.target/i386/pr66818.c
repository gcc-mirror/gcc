/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-sse -mno-mmx -miamcu" } */

struct dummy { int x __attribute__((aligned)); };
int array[__alignof__(struct dummy) == 4 ? 1 : -1];
