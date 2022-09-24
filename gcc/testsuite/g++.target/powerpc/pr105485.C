/* It's to verify no ICE here, ignore error/warning messages
   since they are not test points here.  */
/* { dg-excess-errors "pr105485" } */

template <class> void __builtin_vec_vslv();
typedef  __attribute__((altivec(vector__))) char T;
T b (T c, T d) {
    return __builtin_vec_vslv(c, d);
}
