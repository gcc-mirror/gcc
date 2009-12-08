/* { dg-options "-O3 -fno-inline -fipa-type-escape -fdump-ipa-all -fipa-struct-reorg -fwhole-program -combine" } */
/* { dg-do compile } */
/* { dg-do run } */

#include <stdlib.h>

struct S { int a; struct V *b; };
typedef struct { int c; } T;
typedef struct { int d; int e; } U;

void * 
fn (void *x) 
{
  return x;
}

int
foo (struct S *s)
{
  T x;
  
  T y = *(T *)fn (&x);
  return y.c;
}

int
bar (struct S *s)
{
  U x;
  
  U y = *(U *)fn (&x);
  return y.d + s->a;
}

int 
main ()
{
  struct S s;

  foo(&s) + bar (&s);

  return 0;
}

/*--------------------------------------------------------------------------*/
/* { dg-final { scan-ipa-dump "No structures to transform" "ipa_struct_reorg" { xfail { "avr-*-*" } } } } */
/* { dg-final { cleanup-ipa-dump "*" } } */
