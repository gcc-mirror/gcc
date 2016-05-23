/* PR c/49859 */
/* { dg-do compile } */

extern void foo (int);
extern int j;

void
fn0 (int i)
{
  switch (i)
    {
    int k;
    case 1:
      k = 11;
      foo (k);
    }

  switch (i)
    j = 10; /* { dg-warning "statement will never be executed" } */

  switch (i)
    ;

  switch (i)
    {
    j = 12; /* { dg-warning "statement will never be executed" } */
    default:
      foo (j);
    }

  int o;
  switch (i)
    {
    o = 333; /* { dg-warning "statement will never be executed" } */
    case 4: break;
    default:
      foo (o);
    }

  switch (i)
    switch (j) /* { dg-warning "statement will never be executed" } */
      {
      o = 42; /* { dg-warning "statement will never be executed" } */
      case 8:;
      }

  switch (i)
    {
      int l = 3; /* { dg-warning "statement will never be executed" } */
      o = 5;
      j = 7;
      ++l;
    }

  switch (i)
    {
      int x;
      int l = 3; /* { dg-warning "statement will never be executed" } */
      ++l, ++x;
    }

  switch (i)
    if (j != 3) /* { dg-warning "statement will never be executed" } */
      foo (j);

  switch (i)
    while (1)
     foo (0);

  switch (i)
    while (i > 5) { }

  switch (i)
    goto X; /* { dg-warning "statement will never be executed" } */
X:

  switch (i)
    do
      foo (1);
    while (1);

  switch (i)
    for (;;)
      foo (-1);

  switch (i)
    default:
      j = 6;

  switch (i)
    {
    typedef int T;
    case 3:
      {
	T x = 5;
	foo (x);
      }
    }

  switch (i)
    {
      static int g;
      default:
	foo (g);
    }

  switch (i)
    {
L:
      j = 16;
      default:
	if (j < 5)
	  goto L;
	break;
    }
}
