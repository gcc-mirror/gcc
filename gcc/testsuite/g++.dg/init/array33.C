// PR c++/55804
// { dg-do run }

int t = 0;
template <typename> struct vector {
  vector() { t++; }
};

typedef vector<int> Arrays[1];
class C
{
    vector<int> v_;
    void Foo(const Arrays &);
};
Arrays a;

int main(void)
{
  if (t!=1)
    __builtin_abort ();
  return 0;
}
