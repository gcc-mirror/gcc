/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -Wno-psabi"  } */

typedef char A __attribute__((vector_size (64)));
typedef short B __attribute__((vector_size (64)));
typedef unsigned C __attribute__((vector_size (64)));
typedef long D __attribute__((vector_size (64)));
typedef __int128 E __attribute__((vector_size (64)));

D bar1_D_0;
E bar4 (A, D);

E
bar1 (C C_0)
{
  C_0 >>= 1;
  bar4 ((A) C_0, bar1_D_0);
  bar4 ((A) (E) {~0 }, (D) (A){ ~0 });
  bar4 ((A) (B) { ~0 }, (D) (C) { ~0 });
  bar1 ((C) (D)	{ 0, ~0});
  bar4 ((A) C_0, bar1_D_0);
  (A) { bar1 ((C) { 7})[5] - C_0[63], bar4 ((A) (D) {~0}, (D) (C) { 0, ~0})[3]};
}

E
bar4 (A A_0, D D_0)
{
  bar1 ((C) A_0);
  bar1 ((C) {5});
  bar1 ((C) D_0);
}
