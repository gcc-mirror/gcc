// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontracts-nonattr-client-check=pre" }

struct X
{
    X (int x) noexcept
     pre (x>1)
    {
      try {
       int i = 1;
      }
      catch(...) {
      }
    }
};

int main()
{
  try {
    X x(-42);
  } catch (...) {
  }
}
