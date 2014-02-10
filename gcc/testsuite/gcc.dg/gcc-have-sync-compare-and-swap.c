/* { dg-do link } */
/* MIPS only supports these built-in functions for non-MIPS16 mode, and
   -mflip-mips16 will change the mode of some functions to be different
   from the command-line setting.  */
/* { dg-skip-if "" { mips*-*-* } { "-mflip-mips16" } { "" } } */

void f1()
{
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_1
  typedef int __attribute__  ((__mode__ (__QI__))) qi_int_type;
  qi_int_type qi_int;
  __sync_bool_compare_and_swap (&qi_int, (qi_int_type)0, (qi_int_type)1);
#endif
}

void f2()
{
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
  typedef int __attribute__ ((__mode__ (__HI__))) hi_int_type;
  hi_int_type hi_int;
  __sync_bool_compare_and_swap (&hi_int, (hi_int_type)0, (hi_int_type)1);
#endif
}

void f4()
{
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
  typedef int __attribute__ ((__mode__ (__SI__))) si_int_type;
  si_int_type si_int;
  __sync_bool_compare_and_swap (&si_int, (si_int_type)0, (si_int_type)1);
#endif
}

void f8()
{
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_8
  typedef int __attribute__ ((__mode__ (__DI__))) di_int_type;
  di_int_type di_int;
  __sync_bool_compare_and_swap (&di_int, (di_int_type)0, (di_int_type)1);
#endif
}

/* aligned (16): On S/390 16 byte compare and swap operations are only
   available if the memory operand resides on a 16 byte boundary.  */
void f16()
{
#ifdef __GCC_HAVE_SYNC_COMPARE_AND_SWAP_16
  typedef int __attribute__ ((__mode__ (__TI__), aligned (16))) ti_int_type;
  ti_int_type ti_int;
  __sync_bool_compare_and_swap (&ti_int, (ti_int_type)0, (ti_int_type)1);
#endif
}

int main()
{
  f1();
  f2();
  f4();
  f8();
  f16();  
  return 0;
}
