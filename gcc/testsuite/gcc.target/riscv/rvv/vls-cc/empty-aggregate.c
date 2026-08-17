/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl512b -mabi=lp64d -Wno-psabi" } */

/* An aggregate with no field has nothing to pass in a vector register.  It
   used to run an empty field list into riscv_pass_aggregate_in_vr.  */

struct empty { };
struct empty_array { struct empty a[0]; };

__attribute__((riscv_vls_cc(1024))) void arg (struct empty);
__attribute__((riscv_vls_cc(1024))) void arg_array (struct empty_array);
__attribute__((riscv_vls_cc(1024))) struct empty ret (void);

__attribute__((riscv_vls_cc(1024))) void
def_arg (struct empty x)
{
  arg (x);
}

__attribute__((riscv_vls_cc(1024))) void
def_arg_array (struct empty_array x)
{
  arg_array (x);
}

__attribute__((riscv_vls_cc(1024))) void
call_ret (void)
{
  ret ();
}
