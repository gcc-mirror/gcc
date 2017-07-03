// { dg-do run }
// { dg-shouldfail "asan" }

struct A
{
  int a[5];
};

static __attribute__ ((noinline)) int
goo (A *a)
{
  int *ptr = &a->a[0];
  return *(volatile int *) (ptr - 1);
}

__attribute__ ((noinline)) int
foo (A arg)
{
  return goo (&arg);
}

int
main ()
{
  return foo (A ());
}

// { dg-output "ERROR: AddressSanitizer: stack-buffer-underflow on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size . at.*" }
// { dg-output ".*'arg' <== Memory access at offset \[0-9\]* underflows this variable.*" }
