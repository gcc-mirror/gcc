// PR c++/16630
// { dg-do compile }
// { dg-options "" }
extern "C" int printf (const char*, ...);

template <class T>
struct B { typedef T X; };

template <class U>
struct D
{
  const char* foo (typename B<U>::X) { return __PRETTY_FUNCTION__; }
};

int main ()
{
  printf ("%s\n", D<int>().foo (0));
}
// { dg-final { scan-assembler "const char\\* D<U>::foo\\(typename B<U>::X\\)" } }
