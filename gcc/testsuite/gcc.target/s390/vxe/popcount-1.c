/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=arch12 --save-temps" } */
/* { dg-require-effective-target s390_vxe } */

/* Vectorization currently only works for v4si.  v8hi at least uses 2x
   vpopctf but no vpopcth.  */

typedef unsigned char     uv16qi __attribute__((vector_size(16)));
typedef unsigned short     uv8hi __attribute__((vector_size(16)));
typedef unsigned int       uv4si __attribute__((vector_size(16)));
typedef unsigned long long uv2di __attribute__((vector_size(16)));

uv16qi __attribute__((noinline))
vpopctb (uv16qi a)
{
  uv16qi r;
  int i;

  for (i = 0; i < 16; i++)
    r[i] = __builtin_popcount (a[i]);

  return r;
}
/* { dg-final { scan-assembler "vpopctb\t%v24,%v24" { xfail *-*-* } } } */

uv8hi __attribute__((noinline))
vpopcth (uv8hi a)
{
  uv8hi r;
  int i;

  for (i = 0; i < 8; i++)
    r[i] = __builtin_popcount (a[i]);

  return r;
}
/* { dg-final { scan-assembler "vpopcth\t%v24,%v24" { xfail *-*-* } } } */

uv4si __attribute__((noinline))
vpopctf (uv4si a)
{
  uv4si r;
  int i;

  for (i = 0; i < 4; i++)
    r[i] = __builtin_popcount (a[i]);

  return r;
}
/* { dg-final { scan-assembler "vpopctf\t%v24,%v24" } } */

uv2di __attribute__((noinline))
vpopctg (uv2di a)
{
  uv2di r;
  int i;

  for (i = 0; i < 2; i++)
    r[i] = __builtin_popcount (a[i]);

  return r;
}
/* { dg-final { scan-assembler "vpopctg\t%v24,%v24" { xfail *-*-* } } } */

int
main ()
{
  uv16qi a = (uv16qi){ 42, 1, ~0, 2, 42, 1, ~0, 2, 42, 1, ~0, 2, 42, 1, ~0, 2 };
  if (__builtin_s390_vec_any_ne (vpopctb (a),
				 (uv16qi){ 3, 1, 8, 1, 3, 1, 8, 1,
					   3, 1, 8, 1, 3, 1, 8, 1 }))
    __builtin_abort ();

  if (__builtin_s390_vec_any_ne (vpopcth ((uv8hi){ 42, 1, ~0, 2, 42, 1, ~0, 2 }),
				 (uv8hi){ 3, 1, 16, 1, 3, 1, 16, 1 }))
    __builtin_abort ();

  if (__builtin_s390_vec_any_ne (vpopctf ((uv4si){ 42, 1, ~0, 2 }),
				 (uv4si){ 3, 1, 32, 1 }))
    __builtin_abort ();

  if (__builtin_s390_vec_any_ne (vpopctg ((uv2di){ 42, 1 }),
					  (uv2di){ 3, 1 }))
      __builtin_abort ();


  return 0;
}
