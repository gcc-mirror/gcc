/* This tests aligment propagation to structure elem and
   abcense of redundant & 7.  */

/* { dg-options "-fdump-tree-sanopt" } */
/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

struct st {
  int a;
  int b;
  int c;
} __attribute__((aligned(16)));

int foo (struct st * s_p)
{
  return s_p->a;
}

/* { dg-final { scan-tree-dump-times "& 7" 0 "sanopt" } } */
