// { dg-do compile }
// { dg-options "-O2 -Wstrict-aliasing" }

template<unsigned _Len, unsigned _Align>
struct aligned_storage
{
  union type
    {
      unsigned char __data[_Len];
      struct __attribute__((__aligned__((_Align)))) { } __align;
    };
};

aligned_storage<sizeof(int), __alignof__(int)>::type storage;

int main()
{
  *reinterpret_cast<int*>(&storage) = 42; // { dg-bogus "break strict-aliasing" }
}
