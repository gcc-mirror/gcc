/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fnsplit-blocks-details" } */
void q (void);
int b;
void test (void);
void
split_me (int *a)
{
  if (__builtin_expect (a==0, 0))
    do
    {
      test();
      test();
      test();
      test();
      test();
    }
    while (b);
  else
    q();
}

int
main(void)
{
  int i;
  for (i = 0; i < 1000; i++)
    split_me(&i);
  return 0;
}

/* { dg-final { scan-tree-dump-times "Splitting function at:" 1 "fnsplit"} } */
/* { dg-final { scan-tree-dump-times "Invalid sum" 0 "fnsplit"} } */
