// { dg-do "compile" }
// { dg-options "-std=c++0x" }

template<typename T>
struct vector { 
};

struct X {
  template<typename T>
  struct tmpl { 
    operator T() const;
  };
};

template<typename T>
void g()
{
  T::template tmpl<vector<int>>() + 2;
}

template<typename T>
void operator+(vector<T>, int);

void f()
{
  vector<vector<int>>() + 2;
}
