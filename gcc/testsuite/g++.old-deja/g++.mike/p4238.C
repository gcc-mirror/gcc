// This showed a problem with default op=
// Build don't link:
// prms-id: 4238

struct sigcontext {
  int sc_wbuf[31][25];
};		// gets bogus error - default op= seems broken
