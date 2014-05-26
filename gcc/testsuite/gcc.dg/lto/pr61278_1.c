/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -O1 } } } */

extern char foo (char *);

char d;

int
main ()
{
  foo (&d);
  return 0;
}
