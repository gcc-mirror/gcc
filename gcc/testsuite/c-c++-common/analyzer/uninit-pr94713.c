void f1 (int *);
void f2 (int);

int test_1 (void)
{
  int *p; /* { dg-message "region created on stack here" } */

  f1 (p); /* { dg-warning "use of uninitialized value 'p'" } */
  f1 (p); /* { dg-bogus "use of uninitialized value 'p'" "no followup warnings" } */
  return 0;
}

int test_2 (void)
{
  int *p; /* { dg-message "region created on stack here" } */

  f2 (p[0]); /* { dg-warning "use of uninitialized value 'p'" } */
  return 0;
}
