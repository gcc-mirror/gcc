/* Verify that callgraph construction keeps FOO for possible devirtualization
   and removes BAR.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-visibility"  } */

extern "C" void abort (void);

class A
{
public:
  virtual int foo (void)
     {
	return 4;
     }
  virtual int bar (void)
     {
	return 5;
     }
};


int t(class A *a)
{
  return a->foo();
}
/* { dg-final { scan-ipa-dump "A::foo"  "visibility"  } } */
/* { dg-final { scan-ipa-dump-not "A::bar"  "visibility"  } } */
