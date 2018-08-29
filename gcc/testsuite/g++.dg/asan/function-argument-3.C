// { dg-do run }
// { dg-shouldfail "asan" }
// { dg-additional-options "-Wno-psabi" }

typedef int v4si __attribute__ ((vector_size (16)));

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
  v4si v = {1,2,3,4};
  return foo (v);
}

// { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size . at.*" }
// { dg-output ".*'arg' <== Memory access at offset \[0-9\]* overflows this variable.*" }
