// { dg-do assemble  }
// GROUPS passed references
int func(int& i)
{
  static int& v = i;
  return v;
}

int main()
{
}
