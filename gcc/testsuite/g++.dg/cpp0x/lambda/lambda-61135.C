// { dg-do compile { target c++11 } }

struct Base
{
  virtual int b() const{return 1;};
};

struct Super:Base{};

int main()
{
  constexpr Super s;
  []{s.b();}();  // { dg-error "not captured" }
}
