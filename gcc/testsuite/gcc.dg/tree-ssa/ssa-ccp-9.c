/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Check that cprop works for assignments to array elements and structs.  */

struct foo {
  int a;
};

extern void link_error (void);

void
test9 (struct foo f)
{
  f.a = 0;
  if (f.a != 0)
    link_error ();
}

void
test99 (struct foo *f)
{
  f->a = 0;
  if (f->a != 0)
    link_error ();
}

void
test999 (int *arr)
{
  *arr = 0;
  if (*arr != 0)
    link_error ();
}

void
test9999 (int *arr)
{
  arr[13] = 0;
  if (arr[13] != 0)
    link_error ();
}

void
test99999 (int *arr, int j)
{
  arr[j] = 0;
  if (arr[j] != 0)
    link_error ();
}

/* There should be no link_error calls, if there is any, the
   optimization has failed */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
