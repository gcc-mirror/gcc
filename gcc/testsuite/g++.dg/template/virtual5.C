// PR c++/95158
// { dg-do run }

class Base {
    public:
        virtual void foo()=0;
};

template <typename T>
class MiddleA : virtual public Base {
    public:
        virtual void foo() {}
};

class MiddleB : virtual public Base {};

template <typename T>
class Derived : public MiddleA<T>, public MiddleB {
    public:
        void bar()
        {
	  Derived d;
	  d.foo();
        }
};

int main()
{
  Derived<void> a;
  a.bar(); // Instantiate the template
}
