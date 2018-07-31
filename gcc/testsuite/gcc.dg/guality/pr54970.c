/* PR debug/54970 */
/* PR debug/54971 */
/* { dg-do run } */
/* { dg-options "-g" } */

#include "../nop.h"

int
main ()
{
  int a[] = { 1, 2, 3 };	/* { dg-final { gdb-test .+4 "a\[0\]" "1" } } */
  int *p = a + 2;		/* { dg-final { gdb-test .+3 "a\[1\]" "2" } } */
  int *q = a + 1;		/* { dg-final { gdb-test .+2 "a\[2\]" "3" } } */
				/* { dg-final { gdb-test .+1 "*p" "3" } } */
  asm volatile (NOP);		/* { dg-final { gdb-test . "*q" "2" } } */
  *p += 10;			/* { dg-final { gdb-test .+4 "a\[0\]" "1" } } */
				/* { dg-final { gdb-test .+3 "a\[1\]" "2" } } */
				/* { dg-final { gdb-test .+2 "a\[2\]" "13" } } */
				/* { dg-final { gdb-test .+1 "*p" "13" } } */
  asm volatile (NOP);		/* { dg-final { gdb-test . "*q" "2" } } */
  *q += 10;			/* { dg-final { gdb-test .+4 "a\[0\]" "1" } } */
				/* { dg-final { gdb-test .+3 "a\[1\]" "12" } } */
				/* { dg-final { gdb-test .+2 "a\[2\]" "13" } } */
				/* { dg-final { gdb-test .+1 "*p" "13" } } */
  asm volatile (NOP);		/* { dg-final { gdb-test . "*q" "12" } } */
  __builtin_memcpy (&a, (int [3]) { 4, 5, 6 }, sizeof (a));
				/* { dg-final { gdb-test .+4 "a\[0\]" "4" { xfail { *-*-* } } } } */
				/* { dg-final { gdb-test .+3 "a\[1\]" "5" } } */
				/* { dg-final { gdb-test .+2 "a\[2\]" "6" } } */
				/* { dg-final { gdb-test .+1 "*p" "6" } } */
  asm volatile (NOP);		/* { dg-final { gdb-test . "*q" "5" } } */
  *p += 20;			/* { dg-final { gdb-test .+4 "a\[0\]" "4" { xfail { *-*-* } } } } */
				/* { dg-final { gdb-test .+3 "a\[1\]" "5" } } */
				/* { dg-final { gdb-test .+2 "a\[2\]" "26" } } */
				/* { dg-final { gdb-test .+1 "*p" "26" } } */
  asm volatile (NOP);		/* { dg-final { gdb-test . "*q" "5" } } */
  *q += 20;			/* { dg-final { gdb-test .+8 "a\[0\]" "4" { xfail { *-*-* } } } } */
				/* { dg-final { gdb-test .+7 "a\[1\]" "25" } } */
				/* { dg-final { gdb-test .+6 "a\[2\]" "26" } } */
				/* { dg-final { gdb-test .+5 "*p" "26" } } */
				/* { dg-final { gdb-test .+4 "p\[-1\]" "25" } } */
				/* { dg-final { gdb-test .+3 "p\[-2\]" "4" { xfail { *-*-* } } } } */
				/* { dg-final { gdb-test .+2 "q\[-1\]" "4" { xfail { *-*-* } } } } */
				/* { dg-final { gdb-test .+1 "q\[1\]" "26" } } */
  asm volatile (NOP);		/* { dg-final { gdb-test . "*q" "25" } } */
  return 0;
}
