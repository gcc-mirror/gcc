/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -mpower9-dform -O2 -mlra" } */

#ifndef TYPE
#define TYPE double
#endif

#ifndef TYPE_IN
#define TYPE_IN TYPE
#endif

#ifndef TYPE_OUT
#define TYPE_OUT TYPE
#endif

#ifndef ITYPE
#define ITYPE long
#endif

#ifdef DO_CALL
extern ITYPE get_bits (ITYPE);

#else
#define get_bits(X) (X)
#endif

void test (ITYPE *bits, ITYPE n, TYPE one, TYPE_IN *p, TYPE_OUT *q)
{
  TYPE x_00 = p[ 0];
  TYPE x_01 = p[ 1];
  TYPE x_02 = p[ 2];
  TYPE x_03 = p[ 3];
  TYPE x_04 = p[ 4];
  TYPE x_05 = p[ 5];
  TYPE x_06 = p[ 6];
  TYPE x_07 = p[ 7];
  TYPE x_08 = p[ 8];
  TYPE x_09 = p[ 9];

  TYPE x_10 = p[10];
  TYPE x_11 = p[11];
  TYPE x_12 = p[12];
  TYPE x_13 = p[13];
  TYPE x_14 = p[14];
  TYPE x_15 = p[15];
  TYPE x_16 = p[16];
  TYPE x_17 = p[17];
  TYPE x_18 = p[18];
  TYPE x_19 = p[19];

  TYPE x_20 = p[20];
  TYPE x_21 = p[21];
  TYPE x_22 = p[22];
  TYPE x_23 = p[23];
  TYPE x_24 = p[24];
  TYPE x_25 = p[25];
  TYPE x_26 = p[26];
  TYPE x_27 = p[27];
  TYPE x_28 = p[28];
  TYPE x_29 = p[29];

  TYPE x_30 = p[30];
  TYPE x_31 = p[31];
  TYPE x_32 = p[32];
  TYPE x_33 = p[33];
  TYPE x_34 = p[34];
  TYPE x_35 = p[35];
  TYPE x_36 = p[36];
  TYPE x_37 = p[37];
  TYPE x_38 = p[38];
  TYPE x_39 = p[39];

  TYPE x_40 = p[40];
  TYPE x_41 = p[41];
  TYPE x_42 = p[42];
  TYPE x_43 = p[43];
  TYPE x_44 = p[44];
  TYPE x_45 = p[45];
  TYPE x_46 = p[46];
  TYPE x_47 = p[47];
  TYPE x_48 = p[48];
  TYPE x_49 = p[49];

  ITYPE i;

  for (i = 0; i < n; i++)
    {
      ITYPE bit = get_bits (bits[i]);

      if ((bit & ((ITYPE)1) << 	0) != 0) x_00 += one;
      if ((bit & ((ITYPE)1) << 	1) != 0) x_01 += one;
      if ((bit & ((ITYPE)1) << 	2) != 0) x_02 += one;
      if ((bit & ((ITYPE)1) << 	3) != 0) x_03 += one;
      if ((bit & ((ITYPE)1) << 	4) != 0) x_04 += one;
      if ((bit & ((ITYPE)1) << 	5) != 0) x_05 += one;
      if ((bit & ((ITYPE)1) << 	6) != 0) x_06 += one;
      if ((bit & ((ITYPE)1) << 	7) != 0) x_07 += one;
      if ((bit & ((ITYPE)1) << 	8) != 0) x_08 += one;
      if ((bit & ((ITYPE)1) << 	9) != 0) x_09 += one;

      if ((bit & ((ITYPE)1) << 10) != 0) x_10 += one;
      if ((bit & ((ITYPE)1) << 11) != 0) x_11 += one;
      if ((bit & ((ITYPE)1) << 12) != 0) x_12 += one;
      if ((bit & ((ITYPE)1) << 13) != 0) x_13 += one;
      if ((bit & ((ITYPE)1) << 14) != 0) x_14 += one;
      if ((bit & ((ITYPE)1) << 15) != 0) x_15 += one;
      if ((bit & ((ITYPE)1) << 16) != 0) x_16 += one;
      if ((bit & ((ITYPE)1) << 17) != 0) x_17 += one;
      if ((bit & ((ITYPE)1) << 18) != 0) x_18 += one;
      if ((bit & ((ITYPE)1) << 19) != 0) x_19 += one;

      if ((bit & ((ITYPE)1) << 20) != 0) x_20 += one;
      if ((bit & ((ITYPE)1) << 21) != 0) x_21 += one;
      if ((bit & ((ITYPE)1) << 22) != 0) x_22 += one;
      if ((bit & ((ITYPE)1) << 23) != 0) x_23 += one;
      if ((bit & ((ITYPE)1) << 24) != 0) x_24 += one;
      if ((bit & ((ITYPE)1) << 25) != 0) x_25 += one;
      if ((bit & ((ITYPE)1) << 26) != 0) x_26 += one;
      if ((bit & ((ITYPE)1) << 27) != 0) x_27 += one;
      if ((bit & ((ITYPE)1) << 28) != 0) x_28 += one;
      if ((bit & ((ITYPE)1) << 29) != 0) x_29 += one;

      if ((bit & ((ITYPE)1) << 30) != 0) x_30 += one;
      if ((bit & ((ITYPE)1) << 31) != 0) x_31 += one;
      if ((bit & ((ITYPE)1) << 32) != 0) x_32 += one;
      if ((bit & ((ITYPE)1) << 33) != 0) x_33 += one;
      if ((bit & ((ITYPE)1) << 34) != 0) x_34 += one;
      if ((bit & ((ITYPE)1) << 35) != 0) x_35 += one;
      if ((bit & ((ITYPE)1) << 36) != 0) x_36 += one;
      if ((bit & ((ITYPE)1) << 37) != 0) x_37 += one;
      if ((bit & ((ITYPE)1) << 38) != 0) x_38 += one;
      if ((bit & ((ITYPE)1) << 39) != 0) x_39 += one;

      if ((bit & ((ITYPE)1) << 40) != 0) x_40 += one;
      if ((bit & ((ITYPE)1) << 41) != 0) x_41 += one;
      if ((bit & ((ITYPE)1) << 42) != 0) x_42 += one;
      if ((bit & ((ITYPE)1) << 43) != 0) x_43 += one;
      if ((bit & ((ITYPE)1) << 44) != 0) x_44 += one;
      if ((bit & ((ITYPE)1) << 45) != 0) x_45 += one;
      if ((bit & ((ITYPE)1) << 46) != 0) x_46 += one;
      if ((bit & ((ITYPE)1) << 47) != 0) x_47 += one;
      if ((bit & ((ITYPE)1) << 48) != 0) x_48 += one;
      if ((bit & ((ITYPE)1) << 49) != 0) x_49 += one;
    }

  q[ 0] = x_00;
  q[ 1] = x_01;
  q[ 2] = x_02;
  q[ 3] = x_03;
  q[ 4] = x_04;
  q[ 5] = x_05;
  q[ 6] = x_06;
  q[ 7] = x_07;
  q[ 8] = x_08;
  q[ 9] = x_09;

  q[10] = x_10;
  q[11] = x_11;
  q[12] = x_12;
  q[13] = x_13;
  q[14] = x_14;
  q[15] = x_15;
  q[16] = x_16;
  q[17] = x_17;
  q[18] = x_18;
  q[19] = x_19;

  q[20] = x_20;
  q[21] = x_21;
  q[22] = x_22;
  q[23] = x_23;
  q[24] = x_24;
  q[25] = x_25;
  q[26] = x_26;
  q[27] = x_27;
  q[28] = x_28;
  q[29] = x_29;

  q[30] = x_30;
  q[31] = x_31;
  q[32] = x_32;
  q[33] = x_33;
  q[34] = x_34;
  q[35] = x_35;
  q[36] = x_36;
  q[37] = x_37;
  q[38] = x_38;
  q[39] = x_39;

  q[40] = x_40;
  q[41] = x_41;
  q[42] = x_42;
  q[43] = x_43;
  q[44] = x_44;
  q[45] = x_45;
  q[46] = x_46;
  q[47] = x_47;
  q[48] = x_48;
  q[49] = x_49;
}

/* { dg-final { scan-assembler     "lxsd "   } } */
/* { dg-final { scan-assembler     "stxsd "  } } */
/* { dg-final { scan-assembler-not "mfvsrd " } } */
/* { dg-final { scan-assembler-not "mtvsrd " } } */
