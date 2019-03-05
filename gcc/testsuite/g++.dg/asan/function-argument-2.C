// { dg-do run }
// { dg-shouldfail "asan" }

static __attribute__ ((noinline)) int
goo (int *a)
{
  return *(volatile int *)a;
}

__attribute__ ((noinline)) int
foo (char arg)
{
  return goo ((int *)&arg);
}

int
main ()
{
  return foo (12);
}

// { dg-output "ERROR: AddressSanitizer: stack-buffer-overflow on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size . at.*" }
// { dg-output ".*'arg' \\(line 11\\) <== Memory access at offset \[0-9\]* partially overflows this variable.*" }
