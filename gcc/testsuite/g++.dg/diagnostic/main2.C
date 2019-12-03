int main ();

typedef int (*fptr) ();

void foo (fptr);

fptr bar ()
{
  foo (main);  // { dg-error "8:ISO C\\+\\+ forbids taking address of function" }

  return main;  // { dg-error "10:ISO C\\+\\+ forbids taking address of function" }
}
