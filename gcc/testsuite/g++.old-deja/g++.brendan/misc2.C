// { dg-do assemble  }
// GROUPS passed miscellaneous-bugs
// Should be fixed by:
// Sun Jun 13 12:55:22 1993  Brendan Kehoe  (brendan@lisa.cygnus.com)
//
//	* cp-decl.c (grokvardecl): Don't complain about duplicate
//	definitions of `extern "C"' declarations (parallelize it with how
//	regular `extern' decls are handled).

extern "C" double  _MaXdOuB, _MiNdOuB;
extern "C" double  _MaXdOuB, _MiNdOuB;	// no error should be emitted for this
