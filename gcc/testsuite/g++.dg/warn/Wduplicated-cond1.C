// PR c++/94265
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Wduplicated-cond" }

void
foo ()
{
  if (int a = 0; a)
  { }
  else if (a = 5; a) // { dg-message "previously used here" }
  { }
  else if (; a) // { dg-warning "duplicated .if. condition" }
  { }
  else if (int b = ++a; a)
  { }
}
