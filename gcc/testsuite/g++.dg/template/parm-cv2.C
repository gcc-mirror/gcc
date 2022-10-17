// PR c++/51851

template<class T>
struct A
{
  typedef double Point[2];
  virtual double calculate(const Point point) const = 0;
};

template<class T>
struct B : public A<T>
{
  virtual double calculate(const typename A<T>::Point point) const
  {
    return point[0];
  }
};

int main()
{
  B<int> b;
  return 0;
}
