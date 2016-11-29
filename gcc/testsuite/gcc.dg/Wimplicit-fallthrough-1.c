/* PR c/7652 */
/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough -Wdeclaration-after-statement" } */

/* Test we don't print bogus "mixed declarations and code" warning.  */

int
f (int b)
{
  switch (b)
    {
    case 0:
      b++;
      __attribute__((fallthrough));
    case 1:
      b--;
      __attribute__((unused)) int a; /* { dg-warning "mixed declarations and code" } */
    case 2:
      break;
    }
  return 99;
}
