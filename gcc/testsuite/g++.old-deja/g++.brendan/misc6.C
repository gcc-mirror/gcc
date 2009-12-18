// { dg-do assemble  }
// GROUPS passed miscellaneous
// test that use of `inline' is forbidden when it should be
inline int i;// { dg-error "" } .*
struct c { inline int i; };// { dg-error "" } .*
int foo (inline int i);// { dg-error "" } .*
inline class c; // { dg-error "'inline' can only be specified for functions" } inline
inline typedef int t; // { dg-error "" } inline
class d { inline friend class c; }; // { dg-error "'inline' can only be specified for functions" } inline
