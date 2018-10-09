// { dg-do run }
// { dg-shouldfail "asan" }
// { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } }

int
main (void)
{
  char *ptr;
  char *ptr2;
  {
    char my_char[9];
    ptr = &my_char[0];
    __builtin_memcpy (&ptr2, &ptr, sizeof (ptr2));
  }

  *(ptr2+9) = 'c';
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "WRITE of size 1 at.*" }
// { dg-output ".*'my_char' \\(line 11\\) <== Memory access at offset \[0-9\]* overflows this variable.*" }
