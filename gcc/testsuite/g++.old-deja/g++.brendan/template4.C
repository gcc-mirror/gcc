// { dg-do assemble  }
// GROUPS passed templates
// This used to be a circularity in chainon.
template <class ARG> struct TMPL {
    typedef int ARG::*ARG_data_member_ptr;
    typedef void (ARG::*ARG_func_member_ptr) ();
};

struct S { };

TMPL<S> object;
