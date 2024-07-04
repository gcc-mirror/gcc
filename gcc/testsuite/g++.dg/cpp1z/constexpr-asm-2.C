/* { dg-do compile } */
/* { dg-options "-std=gnu++11" } */

using size_t = typeof(sizeof(0));
template <typename T, size_t N>
struct array {
  constexpr size_t size () const { return N; }
  constexpr const T *data () const { return a; }
  const T a[N];
};

void f()
{
  int a;
  asm((array<char, 3> {'f','o','o'}) :
      (array<char, 2>{'=','r'})  (a) :
      (array<char, 1>{'r'}) (1) :
      (array<char, 6>{'m','e','m','o','r','y'}));
}

/* { dg-final { scan-assembler "foo" } } */
