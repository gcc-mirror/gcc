// Build don't link: 
// Special g++ Options: -g -fexternal-templates
// GROUPS passed templates
// Bug: g++ emits template instances when it shouldn't.
// Special g++ Options: -g -fexternal-templates

#pragma implementation "irrelevant_file"
#line 1 "wa.h"
#pragma interface		// ERROR - , XFAIL *-*-*
template <class T> inline T min (T a, T b) { return a<b?a:b; }
#line 3 "wa.C" 

main()
{
  min (1, 1); 
}// UNKNOWN "min"
