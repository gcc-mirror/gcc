// { dg-do assemble  }
// This showed a problem with default op=
// prms-id: 4238

struct sigcontext {
  int sc_wbuf[31][25];
};		// { dg-bogus "" } default op= seems broken
