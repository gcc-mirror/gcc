template <class T, int (T::*)> struct foo;

template <class T>
int f(foo<T,&T::ob_type>*);

template <class T>
char* f(...);

struct X { int ob_type; };
struct Y { char* ob_type; };
  int x = f<X>(0);
char* y = f<Y>(0);
char* z = f<int>(0);

int main() { return 0; }
