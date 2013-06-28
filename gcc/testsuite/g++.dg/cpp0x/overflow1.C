template <long long i>
struct Fib
{
    static const long long value // { dg-error "overflow" }
    = Fib<i-1>::value + Fib<i-2>::value;
};

template <>
struct Fib<0>
{
   static const long long value = 0;
};

template <>
struct Fib<1>
{
   static const long long value = 1;
};

int main()
{
  return Fib<95>::value;
}
