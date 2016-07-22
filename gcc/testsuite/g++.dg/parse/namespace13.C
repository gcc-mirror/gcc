// PR c++/60385

float foo4();   // { dg-message "previous declaration" }

namespace foo4  // { dg-error "redeclared" }
{
  struct bar6
    {
      friend wchar_t bar1();
    };
}
