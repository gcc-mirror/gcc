// { dg-do run }

extern "C" void abort (void);

template<typename _Tp>
inline const _Tp&
min(const _Tp& __a, const _Tp& __b)
{
  if (__b < __a)
    return __b;
  return __a;
}

struct A
{
  int m_x;

  explicit A(int x) : m_x(x) {}
  operator int() const { return m_x; }
};

struct B : public A
{
public:
  explicit B(int x) : A(x) {}
};

int data = 1;

int main()
{
  B b = B(10);
  b = min(b, B(data));
  if (b != 1)
    abort ();
  return 0;
}
