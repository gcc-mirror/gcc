/* Various tests for loops that aren't infinite, strictly speaking,
   but look bogus.  */

// TODO: should we complain about these?

extern int maybe_useful_work ();

/* Loop iteration going the wrong way, with a signed iterator

   Not infinite, as will eventually overflow and bail out, but probably
   not what the user intended.  */

void test_wrong_way_signed_1 (int n)
{
  for (int i = 0; i < n; i--)
    {
    }
}

void test_wrong_way_signed_2 (int n)
{
  for (int i = 0; i < n; i--)
    maybe_useful_work ();
}

int test_wrong_way_signed_3 (int *arr, int n)
{
  int sum = 0;
  for (int i = 0; i < n; i--)
    sum += arr[i];
  return sum;
}


/* As above, but with an unsigned iterator.

   Not infinite, as will immediately overflow and bail out, but probably
   not what the user intended.  */

void test_wrong_way_unsigned_1 (unsigned n)
{
  for (unsigned i = 0; i < n; i--)
    {
    }
}

void test_wrong_way_unsigned_2 (unsigned n)
{
  for (unsigned i = 0; i < n; i--)
    maybe_useful_work ();
}

int test_wrong_way_unsigned_3 (int *arr, unsigned n)
{
  int sum = 0;
  for (unsigned i = 0; i < n; i--)
    sum += arr[i];
  return sum;
}

/* BUG: "n" never changes, so loop is never entered.  */

void test_1 (void)
{
  int n = 0;

  /* [...snip...]  */

  for (int i = 0; i < n; i++)
    maybe_useful_work ();
}
