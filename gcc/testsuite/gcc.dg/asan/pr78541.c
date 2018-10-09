// PR sanitizer/78560
// { dg-do run }
// { dg-shouldfail "asan" }

void foo (double a, double b)
{
  double *ptr;
    {
      double x = a + b;
      ptr = &x;
    }
 double square () { __builtin_printf ("", *ptr); }

 square ();
}

int main()
{
  foo (1.2f, 2.3f);
  return 0;
}

// { dg-output "ERROR: AddressSanitizer: stack-use-after-scope on address.*(\n|\r\n|\r)" }
// { dg-output "READ of size.*" }
// { dg-output ".*'x' \\(line 9\\) <== Memory access at offset \[0-9\]* is inside this variable.*" }
