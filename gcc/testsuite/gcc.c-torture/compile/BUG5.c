enum bar
{
  one,
  two
};

enum bar foo;

void bar()
{
  switch (foo)
  {
    case one:
    case two:
      __builtin_printf ("one to two\n");
    break;
  }
}
