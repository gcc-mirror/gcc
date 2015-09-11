/* Test whether overloading works as expected.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z13 -mzarch -mzvector" } */

__vector int v4si;
__vector unsigned uv4si;
__vector bool bv4si;
__vector long long v2di;
__vector unsigned long long uv2di;
__vector bool long long bv2di;
__vector double v2df;
int *intptr;
unsigned *uintptr;
double *dblptr;
long long ll;
unsigned long long ull;
const int *cintptr;
long long* llptr;
unsigned long long* ullptr;

void
foo ()
{
  __builtin_s390_vec_scatter_element (v4si,  uv4si, (int*)0, 0); /* ok */
  __builtin_s390_vec_insert_and_zero (intptr); /* ok */

  /* The unsigned pointer must not match the signed pointer.  */
  __builtin_s390_vec_scatter_element (v4si, uv4si, uintptr, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* Make sure signed int pointers don't match unsigned int pointers.  */
  __builtin_s390_vec_scatter_element (bv4si, uv4si, intptr, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* Const pointers do not match unqualified operands.  */
  __builtin_s390_vec_scatter_element (v4si, uv4si, cintptr, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* Volatile pointers do not match unqualified operands.  */
  __builtin_s390_vec_scatter_element (v4si, uv4si, cintptr, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* The third operands needs to be double *.  */
  __builtin_s390_vec_scatter_element (v2df, uv4si, intptr, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* This is an ambigious overload.  */
  __builtin_s390_vec_scatter_element (v4si, uv4si, 0, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* Pointer to vector must not match.  */
  __builtin_s390_vec_scatter_element (v4si, uv4si, &v4si, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  /* Don't accept const int* for int*.  */
  __builtin_s390_vec_scatter_element (v4si,  uv4si, cintptr, 0); /* { dg-error "invalid parameter combination for intrinsic" } */

  __builtin_s390_vec_load_pair (ll, ull); /* { dg-error "ambiguous overload for intrinsic" } */
  __builtin_s390_vec_load_pair (ull, ll); /* { dg-error "ambiguous overload for intrinsic" } */
}
