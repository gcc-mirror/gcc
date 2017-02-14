/* { dg-do compile } */
/* { dg-options "-Wno-shadow" } */

void func() {
  int i;
    {
      int i; /* should not warn */
    }
}
