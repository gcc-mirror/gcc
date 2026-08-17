/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d" } */

/* An aggregate with no field has nothing to pass in a vector register.  It
   used to run an empty field list into riscv_pass_aggregate_in_vr.  */

struct empty { };
struct derived : empty { };

__attribute__((riscv_vls_cc(1024))) void arg (empty);
__attribute__((riscv_vls_cc(1024))) void arg_derived (derived);
__attribute__((riscv_vls_cc(1024))) empty ret ();

__attribute__((riscv_vls_cc(1024))) void
def_arg (empty x)
{
  arg (x);
}

__attribute__((riscv_vls_cc(1024))) void
def_arg_derived (derived x)
{
  arg_derived (x);
}

__attribute__((riscv_vls_cc(1024))) void
call_ret ()
{
  ret ();
}
