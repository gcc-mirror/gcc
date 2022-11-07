/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fpic -fno-plt -mno-explicit-relocs -mcmodel=medium" } */
/* { dg-final { scan-assembler "test:.*la\.global\t.*g\n\tjirl" } } */
/* { dg-final { scan-assembler "test1:.*la\.global\t.*f\n\tjirl" } } */
/* { dg-final { scan-assembler "test2:.*la\.local\t.*l\n\tjirl" } } */
/* { dg-final { scan-assembler "test3:.*la\.global\t.*\_\_tls\_get\_addr" { target tls_native } } } */

extern void g (void);
void
f (void)
{}

static void
l (void)
{}

void
test (void)
{
  g ();
}

void
test1 (void)
{
  f ();
}

void
test2 (void)
{
  l ();
}

__attribute__ ((tls_model ("global-dynamic"))) __thread int a;

void
test3 (void)
{
  a = 10;
}
