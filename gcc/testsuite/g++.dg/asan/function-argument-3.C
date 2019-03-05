// { dg-do run }
// { dg-shouldfail "asan" }
// { dg-additional-options "-Wno-psabi" }

// On SPARC 32-bit, only vectors up to 8 bytes are passed in registers
#if defined(__sparc__) && !defined(__sparcv9) && !defined(__arch64__)
#define SMALL_VECTOR
#endif

#ifdef SMALL_VECTOR
typedef int v4si __attribute__ ((vector_size (8)));
#else
typedef int v4si __attribute__ ((vector_size (16)));
#endif

static __attribute__ ((noinline)) int
goo (v4si *a)
{
  return (*(volatile v4si *) (a + 1))[2];
}

__attribute__ ((noinline)) int
foo (v4si arg)
{
  return goo (&arg);
}

int
main ()
{
#ifdef SMALL_VECTOR
  v4si v = {1,2};
#else
  v4si v = {1,2,3,4};
#endif
  return foo (v);
}

// { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size . at.*" }
// { dg-output ".*'arg' \\(line 23\\) <== Memory access at offset \[0-9\]* overflows this variable.*" }
