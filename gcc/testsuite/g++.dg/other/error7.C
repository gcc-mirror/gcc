// { dg-do compile }
// Origin: <andrewp at andypo dot net>
// c++/8046: ICE on illegal code involving destructor being treated as bit-not
//  expression

class A;
namespace N {}

void foo(void)
{
  N::~A();    // { dg-error "not a member" }
}
