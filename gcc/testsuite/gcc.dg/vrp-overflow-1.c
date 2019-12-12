/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-forwprop" } */

extern void __attribute__((noreturn)) unreachable (void);

int fle22 (int a)
{
  unsigned i = a / 4;
  unsigned j = i - 2;

  if (j == 7) /* A dynamic range excludes a value from j for the rest of f1.  */
    return -1;

  if (i <= 2) /* This dynamic range cannot be combined or compared with that of j.  */
    return 0;

  if (i <= j) /* And so we couldn't compute this result.  */
    unreachable ();

  return 1;
}

int fle32 (int a)
{
  unsigned i = a / 4;
  unsigned j = i - 3;

  if (j == 7) /* A dynamic range excludes a value from j for the rest of f1.  */
    return -1;

  if (i <= 2) /* This dynamic range cannot be combined or compared with that of j.  */
    return 0;

  if (i <= j) /* And so we couldn't compute this result.  */
    unreachable ();

  return 1;
}

int flt22 (int a)
{
  unsigned i = a / 4;
  unsigned j = i - 2;

  if (j == 7)
    return -1;

  if (i <= 2)
    return 0;

  if (i < j)
    unreachable ();

  return 1;
}

int flt32 (int a)
{
  unsigned i = a / 4;
  unsigned j = i - 3;

  if (j == 7)
    return -1;

  if (i <= 2)
    return 0;

  if (i < j)
    unreachable ();

  return 1;
}

int fgt22 (int a)
{
  unsigned i = a / 4;
  unsigned j = i + 2;

  if (j == -7)
    return -1;

  if (i >= -3)
    return 0;

  if (i > j)
    unreachable ();

  return 1;
}

int fgt32 (int a)
{
  unsigned i = a / 4;
  unsigned j = i + 3;

  if (j == -7)
    return -1;

  if (i >= -3)
    return 0;

  if (i > j)
    unreachable ();

  return 1;
}

int fge22 (int a)
{
  unsigned i = a / 4;
  unsigned j = i + 2;

  if (j == -7)
    return -1;

  if (i >= -3)
    return 0;

  if (i >= j)
    unreachable ();

  return 1;
}

int fge32 (int a)
{
  unsigned i = a / 4;
  unsigned j = i + 3;

  if (j == -7)
    return -1;

  if (i >= -3)
    return 0;

  if (i >= j)
    unreachable ();

  return 1;
}

int main (int argc, char *argv[]) {
  fle22 (argc);
  fle32 (argc);
  flt22 (argc);
  flt32 (argc);
  fgt22 (argc);
  fgt32 (argc);
  fge22 (argc);
  fge32 (argc);
}
