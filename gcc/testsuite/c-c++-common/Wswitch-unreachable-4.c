/* { dg-do compile } */

void
foo (int a, int b)
{
  switch (a)
    {
      { int c; }
      { int d; }
      { int e; }
      b++; /* { dg-warning "statement will never be executed" } */
    case 1:
      break;
    }

  switch (a)
    {
      { int c; }
      { int d = 1; } /* { dg-warning "statement will never be executed" } */
      { int e; }
      b++;
    case 1:
      break;
    }
}
