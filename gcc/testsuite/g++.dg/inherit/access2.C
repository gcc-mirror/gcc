// { dg-do compile }

class A {
protected:
  A& operator=( const A& a ) { return *this; }
};

class B : public A
{};

int main()
{
  B x;
  B y;
  x = y;
}
