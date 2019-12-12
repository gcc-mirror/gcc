// { dg-do run }
// { dg-options "-Wno-vla -Wno-stringop-overflow -fsanitize=undefined" }
// { dg-output "index 1 out of bounds" }

void f(int i)
{
  /* The following creates an array of char[4] on the stack and
     the initialization triggers a -Wstringop-overflow with LTO
     (or when the function is inlined into the called, such as
     with -fwhole-program).  See PR91258.  The warning is
     suppressed above.  */
  int ar[i] = { 42, 24 };
}

int main()
{
  f(1);
}
