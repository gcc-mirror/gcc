__attribute__((used)) int val,val2 = 1;

struct foo {int a;};

struct foo **ptr;

__attribute__ ((noipa))
int
test2 (void *a)
{
  ptr = (struct foo **)a;
}
int test3 (void *a);

int
test(void)
{
  struct foo *fp;
  test2 ((void *)&fp);
  fp = (void *) 0;
  (*ptr)++;
  test3 ((void *)&fp);
}

int testb (void);

int
main()
{
  for (int i = 0; i < val2; i++)
  if (val)
    testb ();
  else
    test();
}
