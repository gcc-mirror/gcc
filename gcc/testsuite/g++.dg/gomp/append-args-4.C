/* Check the error when 'omp_interop_t' is not defined and the variant function
   is found via Argument-dependent lookup; in that case, 'g' is not yet resolved
   to a decl but is an indentifier node. Hence, the location is suboptimal, but
   we get at least an error.  */

namespace N {
  class C{
    public:
  };
  void g(C *c);
}

#pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: c) append_args (interop(target))
void f3(N::C *c);

/* { dg-error "30: argument 2 of 'g' must be of 'omp_interop_t'" "" { target *-*-* } .-3 }  */
/* { dg-note "108: 'append_args' specified here" "" { target *-*-* } .-4 }  */
