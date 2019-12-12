// { dg-do run }
// { dg-shouldfail "asan" }

int *bar (int *x, int *y) { return y; }

int foo (void)
{
  char *p;
  {
    char a = 0;
    p = &a;
  }

  if (*p)
    return 1;
  else
    return 0;
}

int
main (void)
{
  char *ptr;
  {
    char my_char[9];
    ptr = &my_char[0];
  }

  int a[16];
  int *p, *q = a;
  {
    int b[16];
    p = bar (a, b);
  }
  bar (a, q);
  {
    int c[16];
    q = bar (a, c);
  }
  int v = *bar (a, q);
  return v;
}


// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size 4 at.*" }
// { dg-output ".*'c' \\(line 37\\) <== Memory access at offset \[0-9\]* is inside this variable.*" }
