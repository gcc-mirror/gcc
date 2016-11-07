// { dg-do run }
// { dg-shouldfail "asan" }

int
main (int argc, char **argv)
{
  int *ptr = 0;

  {
    int a;
    ptr = &a;
    *ptr = 12345;
  }

  return *ptr;
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size .*" }
// { dg-output ".*'a' <== Memory access at offset \[0-9\]* is inside this variable.*" }
