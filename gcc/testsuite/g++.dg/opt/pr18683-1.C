// PR middle-end/18683
// { dg-do compile }
// { dg-options "-O0" }

template<typename _CharT>
struct basic_ostream
{
  basic_ostream& operator<<(int __n);
};

extern basic_ostream<char>  cout;

template<int> struct linear_congruential
{
  template<class CharT>
  friend basic_ostream<CharT>&
  operator<<(basic_ostream<CharT>& os,
             const linear_congruential& lcg)
  {
    return os << 1;
  }
};

void instantiate_all()
{
  linear_congruential<0> lcf;
  cout << lcf;
}

