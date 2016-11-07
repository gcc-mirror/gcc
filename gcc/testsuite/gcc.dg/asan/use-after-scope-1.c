// { dg-do run }
// { dg-shouldfail "asan" }

int
main (void)
{
  char *ptr;
  {
    char my_char[9];
    ptr = &my_char[0];
  }

  return *(ptr+8);
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size 1 at.*" }
// { dg-output ".*'my_char' <== Memory access at offset \[0-9\]* is inside this variable.*" }
