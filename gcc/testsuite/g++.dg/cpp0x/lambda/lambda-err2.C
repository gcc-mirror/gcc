// PR c++/53158
// { dg-do compile { target c++11 } }

int main()
{
  auto a = []() { return true; };
  auto b = []() { return a(); };  // { dg-error "'a' is not captured" }
  int c, d;
  while (b() && c < d)
    {
    }
}
