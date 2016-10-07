/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough" } */

void bar (int);

void
foo (int i)
{
  switch (i)
    {
    case 1:
      bar (1);
      /* FALLTHROUGH */
    case 2:
      bar (2); /* { dg-warning "statement may fall through" } */
    case 3:
      bar (3); /* { dg-warning "statement may fall through" } */
    case 4:
      bar (4);
    default:
      break;
    }
}
