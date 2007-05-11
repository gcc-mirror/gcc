/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo()
{
  int i;
  *(long*)&i = 0;  /* { dg-warning "type-punn" } */
  return i;
}
