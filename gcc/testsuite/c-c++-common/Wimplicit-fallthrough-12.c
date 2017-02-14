/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough -O2" } */

/* Don't let optimizations preclude the warning.  */

extern void bar (int);

void
f (int i)
{
  switch (i)
    {
    case 1:
      if (i > 1)
	bar (1);
      else
	goto D;
      break;
    case 2:
      bar (2); /* { dg-warning "statement may fall through" } */
    D:
    default:
      bar (33);
    }
}
