// { dg-do assemble  }
// { dg-options "" }
// GROUPS passed error-reporting
// Bug: # line directive in template definition interferes with growing obstack
template <class T> class A
{
public:

# 200 "lineno4.C"
      int foo () { undef1(); } // { dg-error "" "" { target *-*-* } 200 }
      // { dg-message "note" "note" { target *-*-* } 200 }
};

template class A<int>;
