module imports.fail21275a;

struct Foo
{
  private int _x;

  private ref int x() return
  {
    return _x;
  }

  int x() const
  {
    return _x;
  }

}

struct Bar
{

  private int _x;

  private int x(int)
  {
    return _x;
  }

  int x() const
  {
    return _x;
  }

}
