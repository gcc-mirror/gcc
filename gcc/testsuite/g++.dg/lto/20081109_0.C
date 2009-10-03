extern "C" { void abort (void);}
int foo (int);

class A
{
  int x;

public:
  A() { x = 2304; }
  ~A() { if (x != 2305) abort (); }
  void inc () { x++; }
};


int main()
{
  A x;
  x.inc();
  try
    {
      foo (0);
      abort ();	// Should not execute
    }
  catch (int e)
    {
      return 0;
    }
}
