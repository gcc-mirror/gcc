/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdbg-cnt=vect_loop:1" } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */
/* { dg-prune-output "dbg_cnt 'vect_loop' set to 0-1" } */

int a, b, g, h;
int c[58];
int d[58];
int fn1() {
  for (; g; g++)
    if (a)
      c[g] = b;
}

int fn2() {
  fn1();
  for (; h; h++)
    d[h] = 0;
}
