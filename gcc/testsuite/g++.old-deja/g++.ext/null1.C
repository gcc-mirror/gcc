// { dg-do run  }
// Test for overloading with g++ NULL.

void f (int *) { }
void f (char, char);
int main ()
{
  f (__null);
}
