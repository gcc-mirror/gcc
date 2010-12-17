// Test for buggy mangling of attribute noreturn in abi<=4
// { dg-options -fabi-version=4 }

void baz (const char *fmt, ...);

// { dg-final { scan-assembler "_Z3barPVFvPKczE" } }
void bar (void (*baz) (const char *fmt, ...)
			   __attribute__ ((noreturn, format (printf, 1, 2))));

void
foo ()
{
  bar (&baz);
}
