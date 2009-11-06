// PR c++/7046

extern "C" int printf (const char *, ...);

#pragma pack(4)

template <typename X >
struct T
{
    char      x1;   /* Usually 3 padding bytes are added after x1 member. */
    int       x2;
};

template <class T>
int f()
{
  struct A { char i1; int i2; };
  return sizeof (A);
}

#pragma pack(1)
template struct T<int>;   /* T<int> is instantiated here */
template int f<int>();

#pragma pack(4)
template struct T<float>; /* T<float> is instantiated here */
template int f<double>();

int main()
{
  printf("sizeof T<int>   = %d\n", sizeof(T<int>));
  printf("sizeof T<float> = %d\n", sizeof(T<float>));
  printf("f<int>()        = %d\n", f<int>());
  printf("f<float>()      = %d\n", f<float>());
  return (sizeof(T<int>) != sizeof(T<float>)
	  || f<int>() != f<float>());
}
