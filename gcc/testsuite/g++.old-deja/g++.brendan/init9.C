// Build don't link: 
// GROUPS passed initialization
int FALSE = 0;
class X {
public:
  static int FALSE;
};

// The compiler should NOT complain about redeclaration of the global
// `FALSE' with this declaration...grokvardecl shouldn't be doing that.
int X::FALSE = 0;
