// PR c++/61135
// { dg-do compile { target c++11 } }

struct Base
{
  virtual int b() const{return 1;};
};
   
struct Super:Base{};
 
int main()
{
  constexpr Super s = Super();
  [&]{s.b();}();
}
