// { dg-do compile }
// { dg-options "-O0 -mzvector -march=arch14 -mzarch" }
// { dg-bogus "internal compiler error" "ICE" { target s390*-*-* } 23 }
// { dg-excess-errors "" }

/* This used to ICE with checking enabled because
   s390_resolve_overloaded_builtin gets called on NON_DEPENDENT_EXPR
   arguments. We then try to determine the type of it, get an error
   node and ICEd consequently when using this.

   This particular instance of the problem disappeared when
   NON_DEPENDENT_EXPRs got removed with:

   commit dad311874ac3b3cf4eca1c04f67cae80c953f7b8
   Author: Patrick Palka <ppalka@redhat.com>
   Date:   Fri Oct 20 10:45:00 2023 -0400

    c++: remove NON_DEPENDENT_EXPR, part 1

   Nevertheless we should check for error mark nodes in that code.  */

template <typename> void foo() {
  __builtin_s390_vec_perm( , , );
}
