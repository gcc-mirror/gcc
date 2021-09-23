/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include "vec-perm-ctor.h"

#include <stdlib.h>

int
main ()
{
  du a_du = 100ULL;
  du b_du = 200ULL;

  di a_di = -100;
  di b_di = 200;

  df a_df = 10.0;
  df b_df = 20.0;

  si a_si = 12;
  si b_si = -25;
  si c_si = -37;
  si d_si = 50;

  sf a_sf = 30.0f;
  sf b_sf = 40.0f;
  sf c_sf = 50.0f;
  sf d_sf = 60.0f;

  hu a_hu = 10;
  hu b_hu = 20;
  hu c_hu = 30;
  hu d_hu = 40;
  hu e_hu = 50;
  hu f_hu = 60;
  hu g_hu = 70;
  hu h_hu = 80;

  qi a_qi = 10;
  qi b_qi = 20;
  qi c_qi = -30;
  qi d_qi = 40;
  qi e_qi = -50;
  qi f_qi = 60;
  qi g_qi = 70;
  qi h_qi = -80;

  v2du res1 = test_ctor_ctor_same_du (a_du, b_du);
  if (res1[0] != a_du || res1[1] != b_du)
    abort ();

  v2df res2 = test_ctor_ctor_same_df (a_df, b_df);
  if (res2[0] != a_df || res2[1] != b_df)
    abort ();

  v4si res3 = test_ctor_ctor_same_si (a_si, b_si, c_si, d_si);
  if (res3[0] != a_si || res3[1] != b_si || res3[2] != c_si || res3[3] != d_si)
    abort ();

  v4sf res4 = test_ctor_ctor_same_sf (a_sf, b_sf, c_sf, d_sf);
  if (res4[0] != a_sf || res4[1] != b_sf || res4[2] != c_sf || res4[3] != d_sf)
    abort ();

  v8hu res5
    = test_ctor_ctor_same_hu (a_hu, b_hu, c_hu, d_hu, e_hu, f_hu, g_hu, h_hu);

  if (res5[0] != a_hu || res5[1] != b_hu || res5[2] != c_hu || res5[3] != d_hu
      || res5[4] != e_hu || res5[5] != f_hu || res5[6] != g_hu
      || res5[7] != h_hu)
    abort ();

  v16qi res6
    = test_ctor_ctor_same_qi (a_qi, b_qi, c_qi, d_qi, e_qi, f_qi, g_qi, h_qi);

  if (res6[0] != a_qi || res6[1] != b_qi || res6[2] != c_qi || res6[3] != d_qi
      || res6[4] != a_qi || res6[5] != b_qi || res6[6] != c_qi
      || res6[7] != d_qi || res6[8] != e_qi || res6[9] != f_qi
      || res6[10] != g_qi || res6[11] != h_qi || res6[12] != e_qi
      || res6[13] != f_qi || res6[14] != g_qi || res6[15] != h_qi)
    abort ();

  v2du res7 = test_ctor_cst_same_du (a_du, b_du);
  if (res7[0] != a_du || res7[1] != 100)
    abort ();

  v4sf res8 = test_ctor_cst_same_sf (a_sf, b_sf);
  if (res8[0] != a_sf || res8[1] != 2.0f || res8[2] != b_sf || res8[3] != 4.0f)
    abort ();

  v2df res9 = test_ctor_cst_same_df (a_df, b_df);
  if (res9[0] != b_df || res9[1] != 200.0)
    abort ();

  v4si res10 = test_cst_ctor_same_si (a_si, b_si);
  if (res10[0] != 1 || res10[1] != 3 || res10[2] != a_si || res10[3] != b_si)
    abort ();

  v2di res11 = test_ctor_cst_diff_di_si (a_di, b_di);
  /* Need to take care of the endianness since the function converts vector
     const to one different vector type (element size), the endianness
     determines the reinterpreted layout.  Same reason for res12 below.  */
  if (res11[0] != -100 ||
#ifdef __LITTLE_ENDIAN__
      res11[1] != 3
#else
      res11[1] != 0x300000000LL
#endif
  )
    abort ();

  v2du res12 = test_cst_ctor_diff_sf_du (a_du, b_du);
  if (
#ifdef __LITTLE_ENDIAN__
    res12[0] != 0x400000003f800000ULL
#else
    res12[0] != 0x3f80000040000000ULL
#endif
    || res12[1] != 100)
    abort ();

  return 0;
}

