// { dg-do assemble  }
// { dg-options "-Wsynth" }

struct A {
  operator int ();
  A& operator= (int);
};

int
main()
{
  A a, b;

  a = b;
}
