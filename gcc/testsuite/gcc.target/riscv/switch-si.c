/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Test for do_tablejump patch.  */
extern void asdf(int);
void foo(int x) {
  switch (x) {
  case 0: asdf(10); break;
  case 1: asdf(11); break;
  case 2: asdf(12); break;
  case 3: asdf(13); break;
  case 4: asdf(14); break;
  }
}
/* { dg-final { scan-assembler-not "srli" } } */
