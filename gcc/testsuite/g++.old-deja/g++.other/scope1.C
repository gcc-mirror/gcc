// { dg-do run  }
// Testcase for proper scoping of local externs.

int x = 1;

int main()
{   
  int x = 2;
  {
    extern int x;
    return (x != 1);
  }
}
