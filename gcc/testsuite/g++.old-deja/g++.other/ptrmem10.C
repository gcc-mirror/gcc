// { dg-do run  }
// Test that we properly convert a constant ptm to bool.

class A { };

int main()
{
  int A::*const p = 0;
  if (p)
    return 1;
}
