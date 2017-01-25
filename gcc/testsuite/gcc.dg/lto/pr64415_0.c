/* { dg-lto-do link } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target shared } */
/* { dg-lto-options { { -O -flto -fpic } } } */
/* { dg-extra-ld-options { -shared } } */
/* { dg-extra-ld-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } } */

extern void bar(char *, int);

extern char *baz;

void foo()
{
  bar(baz, 0);
}
