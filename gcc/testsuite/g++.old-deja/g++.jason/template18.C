// { dg-do link }
// { dg-options "-g -fno-inline -Wno-deprecated -fexternal-templates" }
// GROUPS passed templates
// Bug: g++ emits template instances when it shouldn't.
// Suppressed-instantiation tests like this are known to break on
// Cygwin, because of the MULTIPLE_SYMBOL_SPACES stuff.  This is OK.


#pragma implementation "irrelevant_file"
#line 1 "template18.h"
#pragma interface
template <class T> inline T min (T a, T b) { return a<b?a:b; }
#line 13 "template18.C"

main()
{
  min (1, 1); 	// { dg-error "" "" { target *-*-* } 0 } should produce an undefined symbol error.
}
