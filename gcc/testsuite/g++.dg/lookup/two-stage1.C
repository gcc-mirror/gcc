/* PR c++/2437 */
/* { dg-do run } */

// Test of two-stage name lookup.

int g(double) { return 0; }
 
template <class T> struct X
{
  int f() { return g(2); } // should call g(double)
};
 
inline int g(int) { return 1; }
 
int main()
{
  return X<int>().f(); // should call g(double), but used to call g(int)
}
