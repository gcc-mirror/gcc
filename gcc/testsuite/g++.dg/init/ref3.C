struct Base { };
struct Derived : public Base { };
struct Choose {
  operator Base&();
  operator Derived&();
};

void f()
{
  Choose c;
  Base& ref = c;
}
