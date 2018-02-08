// { dg-do assemble  }
// { dg-options "" }
// { dg-require-effective-target alloca }
// Origin: Alfred Minarik <a8601248@unet.univie.ac.at>

template<typename T>
struct S
{
  void f() {}
};
 
int main()
{
  S<int> s;
  int len = 50;
  char array[len];
  s.f();
}
