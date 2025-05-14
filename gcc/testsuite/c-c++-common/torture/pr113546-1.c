/* { dg-do compile } */
/* { dg-options "-fcompare-debug" } */

int x;
void f() {
fail:
  switch (x) { case 0: goto fail;; }
}
