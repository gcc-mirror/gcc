/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo () {
  int i;
  char* c= reinterpret_cast<char*>(&i);  /* { dg-bogus "char" } */
  c[1] = 1;
  return i;
}
