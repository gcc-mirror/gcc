/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-lto-options { { -O -flto -fpic } } } */
/* { dg-extra-ld-options { -shared } } */

extern void bar(char *, int);

extern char *baz;

void foo()
{
  bar(baz, 0);
}
