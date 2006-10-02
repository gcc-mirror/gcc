// PR c++/29080

struct Base {
  template<class C> void method() { }
};

struct Left : public Base { };
struct Right : public Base { };
struct Join : public Left, public Right { };

void function()
{
  Join join;
  join.Left::method<int>();
}
