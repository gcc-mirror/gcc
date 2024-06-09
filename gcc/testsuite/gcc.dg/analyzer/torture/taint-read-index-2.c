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

int  __attribute__((tainted_args))
test_5 (int idx)
{
  switch (idx)
    {
    default:
      return 0;
    case 5 ... 20:
      return arr[idx]; /* { dg-bogus "bounds checking" } */
      /* 20 is still an out-of-bounds error (off-by-one)
	 but we don't check for that, just that bounds have been imposed.  */

    /* Extra cases to avoid optimizing the switch away.  */
    case 22:
      return 22;
    case 23:
      return -17;
    }
}

int  __attribute__((tainted_args))
test_6 (int idx)
{
  switch (idx)
    {
    default:
      return arr[idx]; /* { dg-warning "without bounds checking" } */

    case 2:
      return arr[idx]; /* { dg-bogus "bounds checking" } */

    case 6 ... 19:
      return arr[idx]; /* { dg-bogus "bounds checking" } */

    case 22:
      return 22;
    case 23:
      return -17;
    }
}

int  __attribute__((tainted_args))
test_7 (int idx)
{
  switch (idx)
    {
    default:
      return arr[idx]; /* { dg-warning "without bounds checking" } */

    case 2 ... 4:
    case 7 ... 9:
      return arr[idx]; /* { dg-bogus "bounds checking" } */

    case 12 ... 19:
      return arr[idx]; /* { dg-bogus "bounds checking" } */

    case 22:
      return 22;
    case 23:
      return -17;
    }
}

int  __attribute__((tainted_args))
test_8 (unsigned idx)
{
  switch (idx)
    {
    default:
      return arr[idx]; /* { dg-warning "without upper-bounds checking" } */

    case 2 ... 4:
    case 7 ... 9:
      return arr[idx]; /* { dg-bogus "bounds checking" } */

    case 12 ... 19:
      return arr[idx]; /* { dg-bogus "bounds checking" } */

    case 22:
      return 22;
    case 23:
      return -17;
    }
}
