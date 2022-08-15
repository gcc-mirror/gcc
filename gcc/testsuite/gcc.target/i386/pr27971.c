/* { dg-do compile } */
/* { dg-options "-O2 -mno-tbm" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } } */

unsigned array[4];

#ifdef _WIN64
__extension__ typedef unsigned long long TYPE;
#else
#define TYPE unsigned long
#endif

unsigned foo(TYPE x)
{
          return array[(x>>2)&3ul];
}

/* { dg-final { scan-assembler-not "shr\[^\\n\]*2" } } */
/* { dg-final { scan-assembler "and\[^\\n\]*12" } } */
