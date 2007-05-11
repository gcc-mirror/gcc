/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo () {
  int i;
  unsigned int* pu = reinterpret_cast<unsigned int*> (&i);  /* { dg-bogus "signed vs. unsigned" } */
  *pu = 1000000;
  return i;
}
