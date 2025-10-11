/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Og" "-Oz" } } */
/* { dg-options "-march=rv64gc_xtheadmemidx" } */

int a;
int **b;

void
c ()
{
  int **e = &b[(unsigned)(long)&a];
  __asm__ ("" : "+A"(*e));
}
