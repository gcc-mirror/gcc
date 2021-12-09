/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O2 -fpic" } */

static void
my_foo (void)
{
}

static void (*resolve_foo (void)) (void)
{
  return my_foo;
}

static void foo (void) __attribute__((ifunc("resolve_foo")));

void *
bar(void)
{
  return foo;
}

/* { dg-final { scan-assembler {leal[ \t]foo@GOTOFF\(%[^,]*\),[ \t]%eax} { target ia32 } } } */
/* { dg-final { scan-assembler {lea(?:l|q)[ \t]foo\(%rip\),[ \t]%(?:e|r)ax} { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "foo@GOT\\\(" { target ia32 } } } */
/* { dg-final { scan-assembler-not "foo@GOTPCREL\\\(" { target { ! ia32 } } } } */
