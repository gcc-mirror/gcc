/* { dg-do compile } */
/* { dg-options "-std=gnu++11 -Wall" } */

typedef float v4f __attribute__((vector_size(4*sizeof(float))));

template <class T> void eat (T&&) {}

void test1 ()
{
  v4f x = {0,1,2,3};
  typedef decltype (x < x) v4i;
  v4i y = {4,5,6,7}; // v4i is not opaque
  eat (y);
}

template<class V>
void test2 ()
{
  V x = {0,1,2,3};
  typedef decltype (x < x) v4i;
  v4i y = {4,5,6,7}; // v4i is not opaque
  eat (y);
}

int main(){
  test1();
  test2<v4f>();
}
