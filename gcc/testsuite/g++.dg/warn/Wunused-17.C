// PR c++/41109, 41110, 41134
// { dg-options "-Wunused" }

int memory_consumption(const int &t) { return sizeof(t); }

int s;
int g() { return memory_consumption(s); }

template <int> struct X { static const int s = 2; };

template <typename T> int f() {
  const unsigned int dim = 2;
  return X<dim>::s;
}

template int f<int>();

static int i;
template <typename> int h() { return i; }
