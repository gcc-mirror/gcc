// { dg-do assemble  }

template <int i> class a
{
public :
int  k;

template <int j> int f() const { return this->f<j-1>(); }

int g() const { return f<i>(); }
};

template <>
template <>
int a<2>::f<0>() const {
  return 0;
}

int main()
{
a<2> x;
return x.g();
}

