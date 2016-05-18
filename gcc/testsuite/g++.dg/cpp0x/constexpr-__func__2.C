// PR c++/70353
// { dg-do link { target c++11 } }

constexpr const char* ce ()
{
  return __func__;
}

const char *c = ce();

int main()
{
}
