/* { dg-do run }  */
/* { dg-options "-O2 -m4 -mlra -ffixed-r7 -ffixed-r8 -ffixed-r9 -ffixed-r10 -ffixed-r11 -ffixed-r12 -ffixed-r13" } */
#include <stdlib.h>
#include <string.h>

typedef struct { int c[64]; } obj;
obj obj0;
obj obj1;

void __attribute__ ((noinline))
bar (int a, int b, int c, int d, obj *q)
{
  if (q->c[0] != 0x12345678 || q->c[1] != 0xdeadbeef) 
    abort ();
}

void foo (obj *p)
{
  obj bobj;
  bobj = *p;
  bar (0, 0, 0, 0, &bobj);
}

int
main ()
{
  obj0.c[0] = 0x12345678;
  obj0.c[1] = 0xdeadbeef;
  foo (&obj0);
  exit (0);
}
