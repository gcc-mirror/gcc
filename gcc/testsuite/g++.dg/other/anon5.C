// PR c++/34094
// { dg-do link { target { ! { *-*-darwin* *-*-hpux* *-*-solaris2.* } } } }
// { dg-options "-gdwarf-2" }
// Ignore additional message on powerpc-ibm-aix
// { dg-prune-output "obtain more information" } */
// Ignore additional messages on Linux/x86 with PIE
// { dg-prune-output "Bad value" } */

namespace {
  struct c
  {
    static const bool t = 0;
  };
}

const bool &f()
{
  return c::t;	// { dg-message "\[Uu\]ndefined" "undefined" { target *-*-* } 0 }
		// Some targets report the error for the previous line, others
		// don't give line number inforamtion for it, so use line 0.
}

int main(void)
{
  return 0;
}

