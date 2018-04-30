// { dg-do compile { target c++11 } }

// DR2061, look inside inline namespace when pushing a namespace.

inline namespace One
{
  namespace Term 
  {
  }
  inline namespace Two
  {
    namespace Space
    {
    }
  }
}

namespace Term
{
  void bob ();
}

namespace Space
{
  void bill ();
}

inline namespace Two
{
  void weed ();
}

void One::Term::bob () {}
void One::Two::Space::bill () {}
void One::Two::weed () {}

void Thing ()
{
  Term::bob ();
  Space::bill ();
  weed ();
}

// { dg-final { scan-assembler "_ZN3One4Term3bobEv:" } }
// { dg-final { scan-assembler "_ZN3One3Two5Space4billEv:" } }
// { dg-final { scan-assembler "_ZN3One3Two4weedEv:" } }
