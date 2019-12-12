template BadImpl(T, alias thename)
{
  void a_bad_idea(T t)
  {
    thename.a_bad_idea(t);
  }
}

class foo
{
  mixin BadImpl!(uint,Mix1) Mix1;
}

int main()
{
  return 0;
}
