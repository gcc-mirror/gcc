// { dg-do assemble  }
// GROUPS passed templates
template <class T>
void foo(T, T*);


void bar()
{
  double d;
  (*((void (*)(int, double*)) (void (*)(int, int*)) &foo<int>))(3, &d);
}
