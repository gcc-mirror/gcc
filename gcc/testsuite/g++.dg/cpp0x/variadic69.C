// { dg-options "-std=gnu++11" }
template<typename T>
struct stored_value
{
  explicit stored_value() : value() { }

  explicit stored_value(const T& value) : value(value) { }

  stored_value(int, const T& value) : value(value) { }

  T value;
};

template<typename... Values>
struct myclass : public stored_value<Values>...
{
  myclass() { }

  explicit myclass(const Values&... values)
    : stored_value<Values>(values)... { }

  explicit myclass(int x, const Values&... values)
    : stored_value<Values>(x, values)... { }

};

void f()
{
  int i;
  float f;
  myclass<int*, float*> ifp1;
  myclass<int*, float*> ifp2(&i, &f);
  myclass<int*, float*> ifp3(1, &i, &f);
}
