// Special g++ Options: -Wsynth

struct A {
  operator int ();
  A& operator= (int);		// WARNING - not used below
};

main()
{
  A a, b;

  a = b;			// WARNING - uses synthesized op=
}
