// Testcase for various invalid gotos.
// Origin: Jason Merrill <jason@redhat.com>
// Build don't link:

void f ()
{
  goto foo1;			 // ERROR - jumps
  try { foo1:; } catch (...) { } // ERROR - into try
  goto foo2;			 // ERROR - jumps
  try { } catch (...) { foo2:; } // ERROR - into catch
  goto foo3;			 // ERROR - jumps
  { int i=2; foo3:; }		 // ERROR - past init

  try { foo4:; } catch (...) { } // ERROR - 
  goto foo4;			 // ERROR - 
  try { } catch (...) { foo5:; } // ERROR - 
  goto foo5;			 // ERROR - 
  { int i=2; foo6:; }		 // ERROR - 
  goto foo6;			 // ERROR - 
}
