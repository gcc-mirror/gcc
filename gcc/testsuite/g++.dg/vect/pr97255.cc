// { dg-require-effective-target c++11 }
// { dg-additional-options "-O3" }

template<class T, unsigned N>
class Array{
public:
    T& operator[](unsigned x) {return m_arr[x];}
private:
    T m_arr[N];
};

int
__attribute__((noipa))
logicalOr(Array< char, 4 > in1[60],
          Array< bool, 4 > out[60])
{
  for (unsigned k0 = 0u; k0 < 60u; ++k0) {
      Array< char, 4 > in1m = in1[k0];
      Array< bool, 4 > x;
      for (unsigned k1 = 0u; k1 < 4u; ++k1) {
          char in1s = in1m[k1];
          x[k1] = in1s != char(0) || in1s != char(0);
      }
      out[k0] = x;
  }
  return out[0][0];
}


int main()
{
  Array< char, 4 > In1[60]{};
  Array< bool, 4 > Out7[60]{};

  for( int i = 0; i < 60; ++i){
      for( int j = 0; j < 4; ++j){
          In1[i][j] = 240 - i*4 - j;
      }
  }

  if (logicalOr(In1, Out7) != 1)
    __builtin_abort ();
  return 0;
}
