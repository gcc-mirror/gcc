// Test that inline redeclarations of builtins are emitted.
// Origin: Roger Sayle  Mar 28, 2002
// Copyright (C) 2002 Free Software Foundation.

namespace std {
   inline int fabs (void) { return 0; }
}


int main ()
{
   return std::fabs ();
}
