// Build don't link: 
// GROUPS passed miscellaneous
// test that use of `inline' is forbidden when it should be
inline int i;// ERROR - .*
struct c { inline int i; };// ERROR - .*
int foo (inline int i);// ERROR - .*
inline class c; // ERROR - inline
inline typedef int t; // ERROR - inline
class d { inline friend class c; }; // ERROR - inline
