struct bar2
{
  int i;
  @disable this();
  this(int i)
  {
    this.i = i;
  }
}

class InnerBar {
  bar2 b;
  
  this()
  {
    b = bar2(0);
  }
}

struct bar1
{
  InnerBar b;
}
  
class Foo
{
  bar1 m_bar1;
}

void main(string[] args)
{
  auto foo = new Foo();
}