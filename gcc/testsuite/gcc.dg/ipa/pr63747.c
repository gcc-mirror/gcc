/* { dg-options "-O2 -fdump-ipa-icf" } */
/* { dg-do run } */

static int __attribute__((noinline))
foo(int i)
{
  switch (i)
  {
    case 0:
    case 1:
    case 2:
    case 3:
      return 0;
    default:
      return 1;
  }
}

static int __attribute__((noinline))
bar(int i)
{
  switch (i)
  {
    case 4:
    case 5:
    case 6:
    case 7:
      return 0;
    default:
      return 1;
  }
}

int main()
{
  return foo(0) + bar(4);
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
