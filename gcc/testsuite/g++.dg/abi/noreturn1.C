// Test that attribute noreturn is not part of the mangled name.
// { dg-options -fabi-version=0 }

void baz (const char *fmt, ...);

// { dg-final { scan-assembler "_Z3barPFvPKczE" } }
void bar (void (*baz) (const char *fmt, ...)
			   __attribute__ ((noreturn, format (printf, 1, 2))));

void
foo ()
{
  bar (&baz);
}
