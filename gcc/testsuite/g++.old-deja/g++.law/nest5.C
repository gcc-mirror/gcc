// Build don't link: 
// GROUPS passed nest
// nest file
// From: tal@vlsi.cs.caltech.edu
// Date:     Mon, 11 Oct 93 16:26:02 -0700
// Subject:  Serious bug: g++2.4.5 -Doesn't support local classes
// Message-ID: <9310112325.AA13386@vlsi.cs.caltech.edu>

void foo() {
  class Wrapper {
  public:
    void F (void * Wrapperptr)
    {
      Wrapper * wrapptr = (  Wrapper  *) Wrapperptr;
    }
  };
}
