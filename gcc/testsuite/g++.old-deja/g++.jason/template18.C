// GROUPS passed templates
// Bug: g++ emits template instances when it shouldn't.
// Special g++ Options: -g -Wno-deprecated -fexternal-templates

// We mark this XFAIL because we can't test for expected linker errors.
// If we get an XPASS for this testcase, that's a bug.
// (OK) excess errors test - XFAIL *-*-*

#pragma implementation "irrelevant_file"
#line 1 "template18.h"
#pragma interface
template <class T> inline T min (T a, T b) { return a<b?a:b; }
#line 13 "template18.C"

main()
{
  min (1, 1); 		// should produce an undefined symbol error.
}
