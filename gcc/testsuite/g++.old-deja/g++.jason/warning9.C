// Special g++ Options: -Wsynth

struct A {
  operator int ();
  A& operator= (int);
};

main()
{
  A a, b;

  a = b;
}
