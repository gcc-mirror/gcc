// PR c++/124983
// { dg-do compile }

template <class T>
struct A
{
  using typename T::Type;

  void f()
  {
    this->type(); // { dg-error "no member named 'type'; did you mean 'Type'" }
  }
};

template <class T>
struct B
{
  typedef typename T::Type Type;
  static T Value;

  void f()
  {
    this->type(); // { dg-error "no member named 'type'; did you mean 'Type'" }
    this->value(); // { dg-error "no member named 'value'; did you mean 'Value'" }
  }
};

struct X
{
  typedef int Type;
};

template <class T>
struct C
{
private:
  typedef typename T::Type Type; // { dg-message "declared private" }
  static T Value; // { dg-message "declared private" }
};

template <class T>
void g()
{
  typename C<T>::Type t; // { dg-error "private within this context" }
  (void) C<T>::Value; // { dg-error "private within this context" }
  (void) t;
}

template void g<X>(); // { dg-message "required from here" }
