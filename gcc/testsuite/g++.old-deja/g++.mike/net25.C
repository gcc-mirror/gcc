// { dg-do assemble  }

void shake_zero()
{
}

void shake_one()
{
}

void (*foo)();

int main(int a)
{
  foo = a ? shake_zero : shake_one;
  return 0;
}
