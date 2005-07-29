/* { dg-do compile } */
/* { dg-options "-O2" } */

class X {
public:
  int mfunc1 () {
    return 1;
  }
  int mfunc2 () {
    return 2;
  }
  X (int a, int b) { }
};

typedef int (X::*memfunc_p_t) ();

memfunc_p_t mf_arr[2] = { &X::mfunc1, &X::mfunc2 };

int
main ()
{
  // Get pntr to the array of pointers to member-funcs
  memfunc_p_t (*mf_arr_p)[2] = &mf_arr;
  // Compare indirect against direct access to an array element
  if ((*mf_arr_p)[0] != mf_arr[0])
    return 1;
  return 0;
}
