// { dg-do assemble  }
// { dg-options "" }
// GROUPS passed error-reporting
// Bug: # line directive gets ignored immediately after text.
template <class T> class A
{
public:
# 200 "lineno2.C"
};

main()
{
   undef1(); // { dg-error "" "" { target *-*-* } 204 }
}
