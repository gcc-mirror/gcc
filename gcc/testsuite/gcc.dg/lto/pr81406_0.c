/* PR lto/81406 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -O2 -g -flto } } } */
/* { dg-extra-ld-options { -g -r -nostdlib -flinker-output=nolto-rel } } */

int a;
int *foo (void);

static inline int __attribute__ ((__artificial__))
bar (void)
{
  if (a)
    *foo () = 2;
}

void *
baz (void)
{
  return (void *) bar;
}
