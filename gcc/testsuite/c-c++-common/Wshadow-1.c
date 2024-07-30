/* { dg-do compile } */
/* { dg-additional-options "-Wshadow=local -Wno-shadow=compatible-local" } */
int c;
void foo(int *c, int *d)   /* { dg-bogus   "Wshadow" } */
{
  int *e = d;
  {
    int d = 0;             /* { dg-warning "Wshadow=local" } */
  }
  {
    int *e = 0;            /* { dg-bogus   "Wshadow=compatible-local" } */
  }
}
#pragma GCC diagnostic warning "-Wshadow"
void bar(int *c, int *d)   /* { dg-warning "Wshadow" } */
{
  int *e = d;
  {
    int d = 0;             /* { dg-warning "Wshadow" } */
  }
  {
    int *e = 0;            /* { dg-warning "Wshadow" } */
  }
}
