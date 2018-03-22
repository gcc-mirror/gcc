// { dg-do run  }
// { dg-options "-Wno-deprecated -fno-exceptions" }
// PRMS Id: 6267

struct A {
  int i;
  A() { i = 2; }
};
 
int
main()
{
  A *p = new A ();
  return 0;
}
