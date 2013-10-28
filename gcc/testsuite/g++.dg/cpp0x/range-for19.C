// PR c++/49838

// { dg-do compile }
// { dg-options "-std=c++11" }

int main()
{
  auto a;        // { dg-error "no initializer" }
  for(auto i: a) // { dg-error "deduce" }
    ;
}
