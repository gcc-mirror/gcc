/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2" } */

double x;

template <typename T>
T *foo(void)
{
  return (T *)&x; /* { dg-warning "strict-aliasing" } */
}

template int *foo<int>(void); /* { dg-message "instantiated from here" } */
template char *foo<char>(void); /* { dg-bogus "instantiated from here" } */

