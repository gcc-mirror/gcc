// Build don't link: 
// Special g++ Options: -fexceptions
// GROUPS passed exceptions
// except file
// Message-Id: <9207221230.AA08566@life.ai.mit.edu>
// From: EWALLACE@endvmtkl.vnet.ibm.com
// Subject: Bugs
// Date: Wed, 22 Jul 92 08:29:30 EDT

extern "C" void puts(const char *);

class foo {
public:
  class error {};

  void cause_error(void) { throw error(); }
};

int main(void)
{
  foo f;
  try {
    f.cause_error();
  }
  catch (foo::error) {
    puts("Caught it.");
  }
  return 0;
}
