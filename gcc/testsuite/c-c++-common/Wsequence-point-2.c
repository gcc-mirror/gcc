/* PR c/69661 - missing -Wsequence-point warning
   { dg-do compile }
   { dg-options "-Wall" } */

int a, b;
short c;

void fn1 (int p) { (void)p; }

void fn2 (void)
{
  fn1(a == (c &= a = b));   /* { dg-warning "\\\[-Wsequence-point\\\]" } */
}
