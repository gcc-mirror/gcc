// { dg-do compile }
// make sure that a typedef for a bool will have the
//  the same results as a bool itself.


typedef volatile bool my_bool;
int main()
{
  my_bool b = false;
  b--; // { dg-error "" }
  return 0;
}

