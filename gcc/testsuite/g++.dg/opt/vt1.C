// Test whether vtable for S is not put into read-only section.
// { dg-do compile }
// { dg-options "-O2 -fpic -fno-rtti" }
// Origin: Jakub Jelinek <jakub@redhat.com>

struct S
{
  virtual void vm (void) {};
} x;

// { dg-final { scan-assembler-not "section.*_ZTV1S.*\"\[^w\"\]*\"" } }
