// PR sanitizer/104449
// { dg-do compile }
// { dg-options "-fexceptions -fsanitize=address -fstack-check=generic" }

void bar (int *);
struct A { A (); ~A (); };

void
foo (int n)
{
  A b;
  {
    int a[n];
    bar (a);
  }
}
