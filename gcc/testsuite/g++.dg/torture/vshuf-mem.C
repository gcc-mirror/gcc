// { dg-options "-std=c++11 -Wno-psabi" }
// { dg-do run }
// { dg-additional-options "-march=z14" { target s390_vxe } }

/* This used to trigger (2024-05-28) the vectorize_vec_perm_const
   backend hook to be invoked with a MEM source operand.  Extracted
   from onnxruntime's mlas library.  */

typedef float V4SF __attribute__((vector_size (16)));
typedef int V4SI __attribute__((vector_size (16)));

template < unsigned I0, unsigned I1, unsigned I2, unsigned I3 > V4SF
MlasShuffleFloat32x4 (V4SF Vector)
{
  return __builtin_shuffle (Vector, Vector, V4SI{I0, I1, I2, I3});
}

int
main ()
{
  V4SF f = { 1.0f, 2.0f, 3.0f, 4.0f };
  if (MlasShuffleFloat32x4 < 1, 1, 1, 1 > (f)[3] != 2.0f)
    __builtin_abort ();
  if (MlasShuffleFloat32x4 < 3, 3, 3, 3 > (f)[1] != 4.0f)
    __builtin_abort ();
  return 0;
}
