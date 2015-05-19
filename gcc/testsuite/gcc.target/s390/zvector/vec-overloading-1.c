/* Test whether overloading works as expected.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-march=z13 -mzarch -mzvector -fdump-tree-original" } */

__vector int var_v4si;
__vector unsigned var_uv4si;
__vector bool var_bv4si;
__vector long long var_v2di;
__vector unsigned long long var_uv2di;
__vector bool long long var_bv2di;
__vector double var_v2df;

int *intptr;
unsigned *uintptr;
double *dblptr;
unsigned long long ull;
const int *cintptr;
long long* llptr;
unsigned long long* ullptr;

typedef __vector int v4si;
typedef __vector unsigned int uv4si;

v4si var2_v4si;
uv4si var2_uv4si;

void
foo ()
{
  __builtin_s390_vec_scatter_element (var_v4si,  var_uv4si, intptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var2_v4si, var2_uv4si, intptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var_bv4si, var_uv4si, uintptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var_uv4si, var_uv4si, uintptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var_v2di,  var_uv2di, llptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var_bv2di, var_uv2di, ullptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var_uv2di, var_uv2di, ullptr, (unsigned long long)0);
  __builtin_s390_vec_scatter_element (var_v2df,  var_uv2di, dblptr, (unsigned long long)0);

  /* While the last argument is a int there is a way to convert it to
     unsigned long long, so this variant is supposed to match.  */
 __builtin_s390_vec_scatter_element (var_v4si,  var_uv4si, intptr, 0);

  __builtin_s390_vec_insert_and_zero (intptr);
  __builtin_s390_vec_insert_and_zero (cintptr);

  __builtin_s390_vec_promote ((signed char)1, 1);
  __builtin_s390_vec_promote ((unsigned char)1, 1);
  __builtin_s390_vec_promote ((short int)1, 1);
  __builtin_s390_vec_promote ((unsigned short int)1, 1);
  __builtin_s390_vec_promote ((int)1, 1);
  __builtin_s390_vec_promote ((unsigned)1, 1);
  __builtin_s390_vec_promote ((long long)1, 1);
  __builtin_s390_vec_promote ((unsigned long long)1, 1);
  __builtin_s390_vec_promote ((double)1, 1);

  /* This is supposed to match vec_promote_s32 */
  __builtin_s390_vec_promote (1, (signed char) -1);

  /* Constants in C usually are considered int.  */
  __builtin_s390_vec_promote (1, 1);

  /* And (unsigned) long if they are too big for int.  */
  __builtin_s390_vec_promote (1ULL << 32, 1);
  __builtin_s390_vec_promote (1LL << 32, 1);
}

/* { dg-final { scan-tree-dump-times "__builtin_s390_vscef " 5 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_s390_vsceg " 4 "original" } } */

/* { dg-final { scan-tree-dump-times "__builtin_s390_vllezf " 2 "original" } } */

/* { dg-final { scan-tree-dump-times "__builtin_s390_vlvgb_noin " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_s390_vlvgh_noin " 2 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_s390_vlvgf_noin " 4 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_s390_vlvgg_noin " 4 "original" } } */
/* { dg-final { scan-tree-dump-times "__builtin_s390_vlvgg_dbl_noin " 1 "original" } } */
