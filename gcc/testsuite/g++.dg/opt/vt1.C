// Test whether vtable for S is not put into read-only section.
// { dg-do compile { target fpic } }
// { dg-options "-O2 -fpic -fno-rtti" }
// { dg-skip-if "requires unsupported run-time relocation" { spu-*-* } { "*" } { "" } }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct S
{
  virtual void vm (void) {};
} x;

// { dg-final { scan-assembler-not "section\[^\n\r\]*_ZTV1S\[^\n\r\]*\"\[^w\"\n\r\]*\"" } }
