// GROUPS passed templates
// Bug: g++ emits template instances when it shouldn't.
// Special g++ Options: -g -fexternal-templates

#pragma implementation "irrelevant_file"
#line 1 "template18.h"
#pragma interface
template <class T> inline T min (T a, T b) { return a<b?a:b; }
#line 12 "template18.C" 

main()
{
  min (1, 1); 		// ERROR - undefined
}
