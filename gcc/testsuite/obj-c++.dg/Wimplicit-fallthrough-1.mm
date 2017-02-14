/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */
/* Test taken from
   <http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0188r0.pdf>.  */

extern void f (int);

void
foo (int n)
{
  switch (n)
    {
    case 22:
    case 33:
      f (1);  /* { dg-warning "statement may fall through" } */
    case 44:
      f (2);
      __attribute__((fallthrough));
    case 55:
      if (n > 10)
	{
	  f (3);
	  break;
	}
      else
	{
	  f (4);
	  __attribute__((fallthrough));
	}
    case 66:
      f (5);
     __attribute__((fallthrough)); /* { dg-warning "not preceding" } */
      f (6); /* { dg-warning "statement may fall through" } */
    case 77:
       f (7);
    }
}
