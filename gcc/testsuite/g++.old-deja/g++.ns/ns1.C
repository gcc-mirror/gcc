// { dg-do run  }
namespace foo{
  int eine_funktion(int)
    {
      return 0;
    }

  int eine_funktion(int,int)
    {
      return 1;
    }
}

int main(int,char**)
{
  return foo::eine_funktion(1);
}
