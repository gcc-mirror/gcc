// GROUPS passed templates
// Bug: g++ emits template instances when it shouldn't.
// Special g++ Options: -g -fexternal-templates

// We mark this XFAIL for the 'collect2: ld returned 1 exit status' message.
// excess errors test - XFAIL *-*-*

#pragma implementation "irrelevant_file"
#line 1 "template18.h"
#pragma interface
template <class T> inline T min (T a, T b) { return a<b?a:b; }
#line 13 "template18.C"

main()
{
  min (1, 1); 		// ERROR - undefined
}
