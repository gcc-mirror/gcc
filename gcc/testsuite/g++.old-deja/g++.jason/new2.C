// PRMS Id: 6267
// Special g++ Options: -Wno-deprecated -fthis-is-variable -fno-exceptions

struct A {
  int i;
  A() { i = 2; }
};
 
main()
{
  A *p = new A ();
}
