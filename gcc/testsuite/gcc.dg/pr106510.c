/* { dg-do compile } */
/* { dg-options "-O2" } */

void foo ();
void ine_ok() {
  float y, x;
  if (x < y || x > y || y)
    foo ();
}

