// GROUPS passed copy-ctors
// Using Cfront 3.0.1 the programm below prints
// 
// 	  A()
// 	  A(const A& a)
// 	  ~A()
// 	  A(A& a)        <---- !!!
// 	  ~A()
// 	  ~A()
// 
// the g++ 2.2.2 (sparc-sun-sunos4.1) generated code prints
// 
// 	  A()
// 	  A(const A& a)
// 	  ~A()
// 	  A(const A& a)  <---- !!!
// 	  ~A()
// 	  ~A()

extern "C" void printf (char *, ...);
extern "C" void exit (int);

int count = 0;

void
die (int x)
{
  if (x != ++count)
    {
      printf ("FAIL\n");
      exit (1);
    }
}

class A {
public:
  A() { die (1); }
  A(const A& a) { die (2); }
  A(A& a) { die (4); }
  ~A() { count++; if (count != 3 && count != 5 && count != 6) die (-1); }
};

void foo1(const A& a) {
  A b = a;
}

void foo2( A& a) {
  A b = a;
}

int main() {
  A a;

  foo1(a);
  foo2(a);

  printf ("PASS\n");
}
