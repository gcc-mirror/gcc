// TODO: remove need for the taint option:
/* { dg-additional-options "-fanalyzer-checker=taint" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#define LOWER_LIMIT 5
#define UPPER_LIMIT 20

static int arr[UPPER_LIMIT];

static int
called_by_test_1 (int iarg)
{
  return arr[iarg]; /* { dg-warning "without bounds checking" } */
}

int __attribute__((tainted_args))
test_1 (unsigned long ularg)
{
  return called_by_test_1 (ularg);
}

static int
called_by_test_2 (int iarg)
{
  if (iarg < LOWER_LIMIT || iarg > UPPER_LIMIT)
    return 0;
  return arr[iarg]; /* { dg-bogus "bounds checking" } */
}

int __attribute__((tainted_args))
test_2 (unsigned long ularg)
{
  return called_by_test_2 (ularg);
}

int __attribute__((tainted_args))
test_3 (int iarg)
{
  if (iarg < LOWER_LIMIT || iarg > UPPER_LIMIT)
    return 0;
  return arr[iarg]; /* { dg-bogus "bounds checking" } */
}

static int
called_by_test_4 (int iarg)
{
  if (iarg < LOWER_LIMIT || iarg > UPPER_LIMIT)
    return 0;
  return arr[iarg]; /* { dg-bogus "bounds checking" } */
}

int __attribute__((tainted_args))
test_4 (unsigned uarg)
{
  return called_by_test_4 (uarg);
}
