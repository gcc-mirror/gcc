/* PR c++/3012 */
/* { dg-do compile } */

class A
{
  public:
    
    template <class T>
    void foo() const
    {
    }
};

template <class T>
class B
{
  public:
    
    void bar(const A& a) const
    {
	// Compile used to fail with parse error before `;' token
	a.foo<double>();
    }
};

int main()
{
    A a;
    B<int> b;
    b.bar(a);
}
