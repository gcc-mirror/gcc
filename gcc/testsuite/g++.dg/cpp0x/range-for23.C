// PR c++/56722
// { dg-do compile { target c++11 } }

int main()
{
  for (const auto& i, 21)  // { dg-error "has no initializer|expected" }
    i;
}
