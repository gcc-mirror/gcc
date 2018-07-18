// { dg-do compile { target c++11 } }

template <class T> struct wrap 
{
  T t;
  wrap(const wrap& other) : t(other.t) {}
};

struct nocopy {
  nocopy (const nocopy&) = delete;
};

int main ()
{
  static_assert(!__is_trivially_constructible(wrap<nocopy>,
					      const wrap<nocopy>&), "");
  return 0;
}
