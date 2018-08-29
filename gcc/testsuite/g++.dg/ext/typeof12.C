// PR c++/71820

void f (void (*) (int, int)) {}

template < typename T > void g (T x, __typeof__ x) {}  // { dg-message "sorry, unimplemented: mangling" }

int main ()
{
  f (g < int >); 
  return 0; 
}
