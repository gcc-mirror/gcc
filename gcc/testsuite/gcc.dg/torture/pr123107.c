/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))

int f( BS_VEC(short, 16)
       BS_TEMP_206)
{
  BS_TEMP_206 = BS_TEMP_206 < 0;
  if (BS_TEMP_206[0] | BS_TEMP_206[1] | BS_TEMP_206[2] | BS_TEMP_206[3]
      | BS_TEMP_206[4] | BS_TEMP_206[5] | BS_TEMP_206[6] | BS_TEMP_206[7]
      | BS_TEMP_206[8] | BS_TEMP_206[9] | BS_TEMP_206[10]
      | BS_TEMP_206[11] | BS_TEMP_206[12] | BS_TEMP_206[13]
      | BS_TEMP_206[14] | BS_TEMP_206[15])
    return 1;
  return 0;
}
