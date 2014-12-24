// PR c++/63985
// { dg-require-effective-target c++11 }

void foo()
{
  int arr;

  for (int i = 5: arr)  // { dg-error "initializer in range-based" }
    ;

  for (int i, j: arr)   // { dg-error "multiple declarations in range-based" }
    ;
}
