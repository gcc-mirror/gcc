// PR c++/49838

// { dg-do compile { target c++11 } }

int main()
{
  auto a;        // { dg-error "no initializer" }
  for(auto i: a) // { dg-error "deduce" }
    ;
}
