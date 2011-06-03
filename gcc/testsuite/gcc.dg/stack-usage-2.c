/* { dg-do compile } */
/* { dg-options "-Wstack-usage=512" } */

int foo1 (void)
{
  char arr[16];
  arr[0] = 1;
  return 0;
} /* { dg-bogus "stack usage" } */

int foo2 (void)
{
  char arr[1024];
  arr[0] = 1;
  return 0;
} /* { dg-warning "stack usage is \[0-9\]* bytes" } */

int foo3 (void)
{
  char arr[1024] __attribute__((aligned (512)));
  arr[0] = 1;
  /* Force dynamic realignment of argument pointer.  */
  __builtin_apply ((void (*)()) foo2, 0, 0);
  return 0;

} /* { dg-warning "stack usage might be \[0-9\]* bytes" } */

int foo4 (int n)
{
  char arr[n];
  arr[0] = 1;
  return 0;
} /* { dg-warning "stack usage might be unbounded" } */
