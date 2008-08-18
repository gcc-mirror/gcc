// PR c++/34094
// { dg-do link { target { ! *-*-hpux* } } }
// { dg-options "-g" }

namespace {
  struct c
  {
    static const bool t = 0;
  };
}

const bool &f()
{
  return c::t;	// { dg-error "undefined" "undefined" { target *-*-* } 0 }
		// Some targets report the error for the previous line, others
		// don't give line number inforamtion for it, so use line 0.
}

int main(void)
{
  return 0;
}

