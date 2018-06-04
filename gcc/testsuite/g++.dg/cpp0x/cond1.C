// PR c++/84588
// { dg-do compile { target c++11 } }

void foo()
{
  if (int bar() {});  // { dg-error "condition declares a function" }

  for (;int bar() {};);  // { dg-error "condition declares a function" }

  while (int bar() {});  // { dg-error "condition declares a function" }

  if (int a[] {});  // { dg-error "condition declares an array" }

  for (;int a[] {};);  // { dg-error "condition declares an array" }

  while (int a[] {});  // { dg-error "condition declares an array" }
}
