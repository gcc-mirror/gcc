// PR c++/49838

// { dg-do compile }
// { dg-options "-std=c++0x" }

int main()
{
  auto a;        // { dg-error "no initializer" }
  for(auto i: a) // { dg-error "deduce" }
    ;
}
