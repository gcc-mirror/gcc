/* { dg-do compile } */
/* { dg-options "-Wstack-usage=512" } */

int foo1 (void)  /* { dg-bogus "stack usage" } */
{
  char arr[16];
  arr[0] = 1;
  return 0;
}

int foo2 (void)  /* { dg-warning "stack usage is \[0-9\]* bytes" } */
{
  char arr[1024];
  arr[0] = 1;
  return 0;
}

int foo3 (void) /* { dg-warning "stack usage might be \[0-9\]* bytes" } */
{
  char arr[1024] __attribute__((aligned (512)));
  arr[0] = 1;
  /* Force dynamic realignment of argument pointer.  */
  __builtin_apply ((void (*)()) foo2, 0, 0);
  return 0;
}

int foo4 (int n) /* { dg-warning "stack usage might be unbounded" } */
{
  char arr[n];
  arr[0] = 1;
  return 0;
}
