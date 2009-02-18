/* Radar 4535968 */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-O0 -gdwarf-2 -dA -fno-eliminate-unused-debug-types" } */
/* { dg-final { scan-assembler "__debug_pubtypes" } } */
/* { dg-final { scan-assembler "long+\[ \t\]+\(0x\)?\[0-9a-f]+\[ \t\n\]+\[#;@]+\[ \t\]+Length of Public Type Names Info" } } */
/* { dg-final { scan-assembler "\"empty\\\\0\"+\[ \t\]+\[#;@]+\[ \t\]+external name" } } */
/* { dg-final { scan-assembler "\"A\\\\0\"+\[ \t\]+\[#;@]+\[ \t\]+external name" } } */
/* { dg-final { scan-assembler "\"B\\\\0\"+\[ \t\]+\[#;@]+\[ \t\]+external name" } } */


struct A
{
  virtual ~A ();
  int a1;
};

A::~A()
{
  a1 = 800;
}

struct B : public A
{
  virtual ~B ();
  int b1;
  int b2;
};

B::~B()
{
  a1 = 900;
  b1 = 901;
  b2 = 902;
}

// Stop the compiler from optimizing away data.
void refer (A *)
{
  ;
}

struct empty {};

// Stop the compiler from optimizing away data.
void refer (empty *)
{
  ;
}

int main (void)
{
  A alpha, *aap, *abp;
  B beta, *bbp;
  empty e;

  alpha.a1 = 100;
  beta.a1 = 200; beta.b1 = 201; beta.b2 = 202;

  aap = &alpha; refer (aap);
  abp = &beta;  refer (abp);
  bbp = &beta;  refer (bbp);
  refer (&e);

  return 0;  // marker return 0
} // marker close brace
