// Keep the source loads of an inline memcmp nontrapping.

// { dg-do compile }
// { dg-options "-O2 -fdelete-null-pointer-checks -fnon-call-exceptions -finline-stringops=memcmp -fdump-rtl-expand" }
// { dg-require-effective-target ptr32plus }
// { dg-skip-if "" keeps_null_pointer_checks }

struct blocks
{
  unsigned char first[16384];
  unsigned char second[16384];
  int compare () const;
};

int
blocks::compare () const
{
  return __builtin_memcmp (first, second, sizeof (first));
}

// { dg-final { scan-rtl-dump {\(mem(?:/[a-z])*/c(?:/[a-z])*:[^\n\r]*\[ this \]} "expand" } }
// { dg-final { scan-assembler-not {\mmemcmp\M} } }
