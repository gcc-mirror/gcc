// PR 31074
// Bug: The reference cast wasn't finding the desired static_cast followed by
// const_cast interpretation.

struct Shape
{
  Shape() {}
  virtual ~Shape() {}
};

struct Loop
{
  Loop() {}
  virtual ~Loop() {}
  virtual void func() {}
};

struct Rect :
  public Shape,
  public Loop
{
  Rect() {}
  virtual ~Rect() {}
};

int main ()
{
  const Rect* rect = new Rect();
  Loop &l = ((Loop&)(*rect));
  return (&l != (const Loop *)rect);
}
