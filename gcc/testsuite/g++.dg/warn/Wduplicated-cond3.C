// PR c++/107593
// { dg-do compile }
// { dg-options "-Wduplicated-cond" }

template <typename T>
void
foo ()
{
  if (T() && T() && int())
    ;
  else if (T() && T() && int())
    ;
}

template <typename T>
void bar(T a)
{
  if (a)
    ;
  else if (a)
    ;
}

template <typename>
void baz(int a)
{
  if (a)
    ;
  else if (a) // { dg-warning "duplicated" }
    ;
}
void
f ()
{
  foo<int>();
  bar(1);
  baz<int>(1);
}
