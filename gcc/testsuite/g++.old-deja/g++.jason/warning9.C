// { dg-do assemble  }
// { dg-options "-Wsynth" }

struct A {
  operator int ();
  A& operator= (int);		// { dg-warning "" } not used below
};

main()
{
  A a, b;

  a = b;			// { dg-warning "" } uses synthesized op=
}
