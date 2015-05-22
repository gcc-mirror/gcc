// PR c++/65598
// { dg-do compile { target c++11 } }

struct ExplicitTest
{
  explicit operator bool() const;
};

explicit ExplicitTest::operator bool() const  // { dg-error "1:'explicit' outside class declaration" }
{
  return true;
}
