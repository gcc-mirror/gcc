// PR c++/84588

void foo()
{
  if (int x);  // { dg-error "expected initializer" }

  for (;int x;);  // { dg-error "expected initializer" }

  while (int x);  // { dg-error "expected initializer" }
}
