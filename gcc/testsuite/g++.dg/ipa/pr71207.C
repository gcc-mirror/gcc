/* PR ipa/71207 */
/* { dg-do run } */

class Class1
{
public:
  Class1() {};
  virtual ~Class1() {};

protected:
  unsigned Field1;
};

class Class2 : public virtual Class1
{
};

class Class3 : public virtual Class1
{
public:
  virtual void Method1() = 0;

  void Method2()
  {
    Method1();
  }
};

class Class4 : public Class2, public virtual Class3
{
public:
  Class4() {};
  virtual void Method1() {};
};

int main()
{
  Class4 var1;
  var1.Method2();

  return 0;
}
