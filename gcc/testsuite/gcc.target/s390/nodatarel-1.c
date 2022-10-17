/* Test -mno-pic-data-is-text-relative option.  No relative addressing
   of elements in .data and .bss are allowed with that option.  */

/* { dg-do compile } */
/* { dg-options "-O3 -fno-optimize-sibling-calls -fpic -mno-pic-data-is-text-relative -march=z10 -mtune=z9-109 -mzarch" } */

static int a = 3;

/* With -mno-pic-data-is-text-relative these must be addressed via
   GOT.  */

int __attribute__((noinline,noclone))
foo ()
{
  return a;
}

/* Just to make a potentially modified.  */

void
bar (int b)
{
  a = b;
}

/* { dg-final { scan-assembler-times "\\.LANCHOR\\d+@GOTENT" 2 } } */

/* The exrl target is a label_ref which should not be affected at
   all.  */

void
mymemcpy (char *dst, char *src, long size)
{
  __builtin_memcpy (dst, src, size);
}

/* { dg-final { scan-assembler "exrl" } } */


/* PLT slots can still be addressed relatively.  */

int
callfoo ()
{
  return foo ();
}

/* { dg-final { scan-assembler-times "foo@PLT" 1 } } */


/* GOT entries can still be addressed relatively.  */

void *
fooptr ()
{
  return &foo;
}

/* { dg-final { scan-assembler-times "foo@GOTENT" 1 } } */
