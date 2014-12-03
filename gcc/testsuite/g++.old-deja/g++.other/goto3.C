// { dg-do assemble  }
// Testcase for various invalid gotos.
// Origin: Jason Merrill <jason@redhat.com>

void f ()
{
  goto foo1;			 // { dg-message "" } jumps
  try { foo1:; } catch (...) { } // { dg-error "" } into try
  goto foo2;			 // { dg-message "" } jumps
  try { } catch (...) { foo2:; } // { dg-error "" } into catch
  goto foo3;			 // { dg-message "" } jumps
  { int i=2; foo3:; }		 // { dg-error "" } past init

  try { foo4:; } catch (...) { } // { dg-error "" } 
  goto foo4;			 // { dg-message "" } 
  try { } catch (...) { foo5:; } // { dg-error "" } 
  goto foo5;			 // { dg-message "" } 
  { int i=2; foo6:; }		 // { dg-error "" } 
  goto foo6;			 // { dg-message "" } 
}
