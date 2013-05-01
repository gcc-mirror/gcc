// PR c++/57132

template<unsigned m, unsigned a>
struct mod 
{
  static unsigned calc(unsigned x) {
    unsigned res = a * x;
    if (m)
      res %= m;
    return res;
  }
};

int main()
{
  mod<3,2>::calc(7);
  mod<0,2>::calc(7);
}
