struct foo {};
typedef long unsigned int & (foo::*pmf)(void);
void fn (...) {}
int main ()
{
  pmf y = 0;
  fn (y);
}
