void f1 (int *);
void f2 (int);

int foo (void)
{
  int *p;

  f1 (p); /* { dg-warning "use of uninitialized value 'p'" } */
  f2 (p[0]); /* { dg-warning "use of uninitialized value 'p'" } */
  return 0;
}
