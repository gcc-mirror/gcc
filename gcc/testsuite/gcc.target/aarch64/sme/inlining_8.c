/* { dg-options "" } */

inline void __attribute__((always_inline))
shared_callee () [[arm::inout("za")]] {} // { dg-error "inlining failed" }

[[arm::new("za")]] inline void __attribute__((always_inline))
new_callee () {} // { dg-error "inlining failed" }

inline void __attribute__((always_inline))
normal_callee () {}

inline void __attribute__((always_inline))
shared_asm_callee () [[arm::inout("za")]] { asm volatile ("" ::: "za"); } // { dg-error "inlining failed" }

[[arm::new("za")]] inline void __attribute__((always_inline))
new_asm_callee () { asm volatile ("" ::: "za"); } // { dg-error "inlining failed" }

inline void __attribute__((always_inline))
normal_asm_callee () { asm volatile ("" ::: "za"); }

void
normal_caller ()
{
  shared_callee ();
  new_callee ();
  normal_callee ();

  shared_asm_callee ();
  new_asm_callee ();
  normal_asm_callee ();
}
