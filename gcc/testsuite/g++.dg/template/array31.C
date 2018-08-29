int *g();

template <class T> 
void f(int i)
{
  int *p = &g()[3];
}
