// { dg-do run }
// { dg-shouldfail "asan" }

#include <stdio.h>

struct Test
{
  Test ()
    {
      my_value = 0;
    }

  ~Test ()
    {
      fprintf (stderr, "Value: %d\n", *my_value);
    }

  void init (int *v)
    {
      my_value = v;
    }

  int *my_value;
};

int main(int argc, char **argv)
{
  Test t;

  {
    int x = argc;
    t.init(&x);
  }

  return 0;
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size 4 at.*" }
// { dg-output ".*'x' \\(line 31\\) <== Memory access at offset \[0-9\]* is inside this variable.*" }
