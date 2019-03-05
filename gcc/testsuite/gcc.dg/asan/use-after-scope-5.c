// { dg-do run }
// { dg-shouldfail "asan" }

int *ptr;

__attribute__((always_inline))
inline static void
foo(int v)
{
  int values[10];
  for (unsigned i = 0; i < 10; i++)
    values[i] = v;

  ptr = &values[3];
}

int
main (int argc, char **argv)
{
  foo (argc);

  return *ptr;
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size 4 at.*" }
// { dg-output ".*'values' \\(line 10\\) <== Memory access at offset \[0-9\]* is inside this variable.*" }
