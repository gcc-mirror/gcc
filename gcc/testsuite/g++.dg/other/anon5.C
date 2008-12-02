// PR c++/34094
// The error is reported for mips*-elf* targets, but as of binutils CVS
// 2008-02-19, the linker cannot find the associated source line.  The
// problem is that mips*-elf tests run from KSEG0 (which is in the upper
// half of the address range), and the linker compares sign-extended
// addresses from .debug_aranges with unextended addresses.
// { dg-do link { target { ! { hppa*-*-hpux* *-*-solaris2.* mips*-*-elf* } } } }
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
		// don't give line number information for it, so use line 0.
}

int main(void)
{
  return 0;
}

