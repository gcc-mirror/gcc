// PRMS Id: 6267
// Special g++ Options: -Wno-deprecated -fno-exceptions

struct A {
  int i;
  A() { i = 2; }
};
 
main()
{
  A *p = new A ();
}
