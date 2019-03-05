/* { dg-do run { target { powerpc*-*-* && { lp64 && p8vector_hw } } } } */
/* { dg-options "-mdejagnu-cpu=power8  -O3" } */

#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

/* Verify vec_revb builtins */

int
main()
{
  int i;
  vector bool char arg_bc, result_bc, expected_bc;
  vector unsigned char arg_uc, result_uc, expected_uc;
  vector signed char arg_sc, result_sc, expected_sc;

  vector bool short int arg_bsi, result_bsi, expected_bsi;
  vector unsigned short int arg_usi, result_usi, expected_usi;
  vector short int arg_si, result_si, expected_si;

  vector bool int arg_bi, result_bi, expected_bi;
  vector unsigned int arg_ui, result_ui, expected_ui;
  vector int arg_int, result_int, expected_int;

  vector bool long long int arg_blli, result_blli, expected_blli;
  vector unsigned long long int arg_ulli, result_ulli, expected_ulli;
  vector long long int arg_lli, result_lli, expected_lli;

  vector __uint128_t arg_uint128, result_uint128, expected_uint128;
  vector __int128_t arg_int128, result_int128, expected_int128;

  vector float arg_f, result_f, expected_f;
  vector double arg_d, result_d, expected_d;

  /* 8-bit ints */
  /* The element is a byte.  Reversing the byte in each byte element
     gives the same value.  */
  arg_bc = (vector bool char) {0x01, 0x23, 0x45, 0x67,
			       0x7E, 0x7C, 0x7A, 0x78,
			       0x02, 0x46, 0x7A, 0x7E,
			       0x13, 0x57, 0x7B, 0x7F};
  expected_bc = arg_bc;

  result_bc = vec_revb (arg_bc);

  for (i = 0; i < 16; i++) {
    if (result_bc[i] != expected_bc[i])
#ifdef DEBUG
      printf("arg_bc[%d] = 0x%x, result_bc[%d] = 0x%x, expected_bc[%d] = 0x%x\n",
	     i, arg_bc[i], i, result_bc[i], i, expected_bc[i]);
#else
      abort();
#endif
  }

  arg_uc = (vector unsigned char) {0x01, 0x23, 0x45, 0x67,
				   0x7E, 0x7C, 0x7A, 0x78,
				   0x02, 0x46, 0x7A, 0x7E,
				   0x13, 0x57, 0x7B, 0x7F};
  expected_uc = arg_uc;

  result_uc = vec_revb (arg_uc);

  for (i = 0; i < 16; i++) {
    if (result_uc[i] != expected_uc[i])
#ifdef DEBUG
      printf("arg_uc[%d] = 0x%x, result_uc[%d] = 0x%x, expected_uc[%d] = 0x%x\n",
	     i, arg_uc[i], i, result_uc[i], i, expected_uc[i]);
#else
      abort();
#endif
  }

  arg_sc = (vector signed char) {0x01, 0x23, 0x45, 0x67,
				 0x7E, 0x7C, 0x7A, 0x78,
				 0x02, 0x46, 0x7A, 0x7E,
				 0x13, 0x57, 0x7B, 0x7F};
  expected_sc = arg_sc;

  result_sc = vec_revb (arg_sc);

  for (i = 0; i < 16; i++) {
    if (result_sc[i] != expected_sc[i])
#ifdef DEBUG
	printf("arg_sc[%d] = 0x%x, result_sc[%d] = 0x%x, expected_sc[%d] = 0x%x\n",
	     i, arg_sc[i], i, result_sc[i], i, expected_sc[i]);
#else
	abort();
#endif
  }

  /* 16-bit ints */
  arg_bsi = (vector bool short int) {0x0123, 0x4567, 0xFEDC, 0xBA98, 0x0246,
				     0x8ACE, 0x1357, 0x9BDF};
  expected_bsi = (vector bool short int) {0x2301, 0x6745, 0xDCFE, 0x98BA,
					  0x4602, 0xCE8A, 0x5713, 0xDF9B};

  result_bsi = vec_revb (arg_bsi);

  for (i = 0; i < 8; i++) {
    if (result_bsi[i] != expected_bsi[i])
#ifdef DEBUG
	printf("arg_bsi[%d] = 0x%x, result_bsi[%d] = 0x%x, expected_bsi[%d] = 0x%x\n",
	      i, arg_bsi[i], i, result_bsi[i], i, expected_bsi[i]);
#else
	abort();
#endif
  }

  arg_usi = (vector unsigned short int) {0x0123, 0x4567, 0xFEDC, 0xBA98,
					 0x0246, 0x8ACE, 0x1357, 0x9BDF};
  expected_usi = (vector unsigned short int) {0x2301, 0x6745, 0xDCFE, 0x98BA,
					      0x4602, 0xCE8A, 0x5713, 0xDF9B};

  result_usi = vec_revb (arg_usi);

  for (i = 0; i < 8; i++) {
    if (result_usi[i] != expected_usi[i])
#ifdef DEBUG
	printf("arg_usi[%d] = 0x%x, result_usi[%d] = 0x%x, expected_usi[%d] = 0x%x\n",
	     i, arg_usi[i], i, result_usi[i], i, expected_usi[i]);
#else
	abort();
#endif
  }

  arg_si = (vector short int) {0x0123, 0x4567, 0xFEDC, 0xBA98, 0x0246, 0x8ACE,
			       0x1357, 0x9BDF};
  expected_si = (vector short int) {0x2301, 0x6745, 0xDCFE, 0x98BA, 0x4602,
				    0xCE8A, 0x5713, 0xDF9B};

  result_si = vec_revb (arg_si);

  for (i = 0; i < 8; i++) {
    if (result_si[i] != expected_si[i])
#ifdef DEBUG
	printf("arg_si[%d] = 0x%x, result_si[%d] = 0x%x, expected_si[%d] = 0x%x\n",
	     i, arg_si[i], i, result_si[i], i, expected_si[i]);
#else
	abort();
#endif
  }

  /* 32-bit ints */
  arg_bi = (vector bool int) {0x01234567, 0xFEDCBA98, 0x02468ACE, 0x13579BDF};
  expected_bi = (vector bool int) {0x67452301, 0x98BADCFE, 0xCE8A4602,
				   0xDF9B5713};

  result_bi = vec_revb (arg_bi);

  for (i = 0; i < 4; i++) {
    if (result_bi[i] != expected_bi[i])
#ifdef DEBUG
	printf("arg_bi[%d] = 0x%x, result_bi[%d] = 0x%x, expected_bi[%d] = 0x%x\n",
	     i, arg_bi[i], i, result_bi[i], i, expected_bi[i]);
#else
	abort();
#endif
  }

  arg_ui = (vector unsigned int) {0x01234567, 0xFEDCBA98, 0x02468ACE,
				  0x13579BDF};
  expected_ui = (vector unsigned int) {0x67452301, 0x98BADCFE, 0xCE8A4602,
				       0xDF9B5713};

  result_ui = vec_revb (arg_ui);

  for (i = 0; i < 4; i++) {
    if (result_ui[i] != expected_ui[i])
#ifdef DEBUG
	printf("arg_ui[%d] = 0x%x, result_ui[%d] = 0x%x, expected_ui[%d] = 0x%x\n",
	     i, arg_ui[i], i, result_ui[i], i, expected_ui[i]);
#else
	abort();
#endif
  }

  arg_int = (vector int) {0x01234567, 0xFEDCBA98, 0x02468ACE, 0x13579BDF};
  expected_int = (vector int) {0x67452301, 0x98BADCFE, 0xCE8A4602, 0xDF9B5713};

  result_int = vec_revb (arg_int);

  for (i = 0; i < 4; i++) {
    if (result_int[i] != expected_int[i])
#ifdef DEBUG
	printf("arg_int[%d] = 0x%x, result_int[%d] = 0x%x, expected_int[%d] = 0x%x\n",
	     i, arg_int[i], i, result_int[i], i, expected_int[i]);
#else
	abort();
#endif
  }

  /* 64-bit ints */
  arg_blli = (vector bool long long int) {0x01234567FEDCBA98,
					  0x02468ACE13579BDF};
  expected_blli = (vector bool long long int) {0x98BADCFE67452301,
					       0xDF9B5713CE8A4602};

  result_blli = vec_revb (arg_blli);

  for (i = 0; i < 2; i++) {
    if (result_blli[i] != expected_blli[i])
#ifdef DEBUG
	printf("arg_blli[%d] = 0x%x, result_blli[%d] = 0x%llx, expected_blli[%d] = 0x%llx\n",
	     i, arg_blli[i], i, result_blli[i], i, expected_blli[i]);
#else
	abort();
#endif
  }

  arg_ulli = (vector unsigned long long int) {0x01234567FEDCBA98,
					      0x02468ACE13579BDF};
  expected_ulli = (vector unsigned long long int) {0x98BADCFE67452301,
						   0xDF9B5713CE8A4602};

  result_ulli = vec_revb (arg_ulli);

  for (i = 0; i < 2; i++) {
    if (result_ulli[i] != expected_ulli[i])
#ifdef DEBUG
	printf("arg_ulli[%d] = 0x%x, result_ulli[%d] = 0x%llx, expected_ulli[%d] = 0x%llx\n",
	     i, arg_ulli[i], i, result_ulli[i], i, expected_ulli[i]);
#else
	abort();
#endif
  }

  arg_lli = (vector long long int) {0x01234567FEDCBA98, 0x02468ACE13579BDF};
  expected_lli = (vector long long int) {0x98BADCFE67452301,
					 0xDF9B5713CE8A4602};

  result_lli = vec_revb (arg_lli);

  for (i = 0; i < 2; i++) {
    if (result_lli[i] != expected_lli[i])
#ifdef DEBUG
	printf("arg_lli[%d] = 0x%x, result_lli[%d] = 0x%llx, expected_lli[%d] = 0x%llx\n",
	     i, arg_lli[i], i, result_lli[i], i, expected_lli[i]);
#else
	abort();
#endif
  }

  /* 128-bit ints */
  arg_uint128[0] = 0x1627384950617243;
  arg_uint128[0] = arg_uint128[0] << 64;
  arg_uint128[0] |= 0x9405182930415263;
  expected_uint128[0] = 0x6352413029180594;
  expected_uint128[0] = expected_uint128[0] << 64;
  expected_uint128[0] |= 0x4372615049382716;

  result_uint128 = vec_revb (arg_uint128);

  if (result_uint128[0] != expected_uint128[0])
    {
#ifdef DEBUG
       printf("result_uint128[0] doesn't match expected_u128[0]\n");
       printf("arg_uint128[0] =  %llx ", arg_uint128[0] >> 64);
       printf(" %llx\n",	 arg_uint128[0] & 0xFFFFFFFFFFFFFFFF);

       printf("result_uint128[0] =  %llx ", result_uint128[0] >> 64);
       printf(" %llx\n", result_uint128[0] & 0xFFFFFFFFFFFFFFFF);

       printf("expected_uint128[0] =  %llx ", expected_uint128[0] >> 64);
       printf(" %llx\n", expected_uint128[0] & 0xFFFFFFFFFFFFFFFF);
#else
       abort();
#endif
    }

  arg_int128[0] = 0x1627384950617283;
  arg_int128[0] = arg_int128[0] << 64;
  arg_int128[0] |= 0x9405182930415263;
  expected_int128[0] = 0x6352413029180594;
  expected_int128[0] = expected_int128[0] << 64;
  expected_int128[0] |= 0x8372615049382716;;

  result_int128 = vec_revb (arg_int128);

  if (result_int128[0] != expected_int128[0])
    {
#ifdef DEBUG
       printf("result_int128[0] doesn't match expected128[0]\n");
       printf("arg_int128[0] =  %llx ", arg_int128[0] >> 64);
       printf(" %llx\n",	 arg_int128[0] & 0xFFFFFFFFFFFFFFFF);

       printf("result_int128[0] =  %llx ", result_int128[0] >> 64);
       printf(" %llx\n", result_int128[0] & 0xFFFFFFFFFFFFFFFF);

       printf("expected_int128[0] =  %llx ", expected_int128[0] >> 64);
       printf(" %llx\n", expected_int128[0] & 0xFFFFFFFFFFFFFFFF);
#else
       abort();
#endif
    }

  /* 32-bit floats */
  /* 0x42f7224e, 0x43e471ec, 0x49712062, 0x4a0f2b38 */
  arg_f = (vector float) {123.567, 456.89, 987654.123456, 2345678.0};
  /* 0x4e22F742, 0xec71e443, 0x62207149, 0x382b0f4a */
  expected_f = (vector float) {683528320.0,
			       -1169716232068291395011477504.0,
			       739910526898278498304.0,
			       0.0000407838160754181444644927978515625};

  result_f = vec_revb (arg_f);

  for (i = 0; i < 4; i++) {
    if (result_f[i] != expected_f[i])
#ifdef DEBUG
	printf("    arg_f[%d] = %f, result_f[%d] = %f, expected_f[%d] = %f\n",
	     i, arg_f[i], i, result_f[i], i, expected_f[i]);
#else
	abort();
#endif
  }

  /* 64-bit floats */
  /* 0x419D6F34547E6B75   0x4194E5FEC781948B */
  arg_d = (vector double) {123456789.123456789, 87654321.87654321};
  /* 0x756B7E54346F9D41   0x8B9481C7FEE59441 */
  expected_d = (vector double) {4.12815412905659550518671402044E257,
				-6.99269992046390236552018719554E-253};

  result_d = vec_revb (arg_d);

  for (i = 0; i < 2; i++) {
    if (result_d[i] != expected_d[i])
#ifdef DEBUG
	printf("arg_d[%d] = %f, result_d[%d] = %f, expected_d[%d] = %f\n",
	     i, arg_d[i], i, result_d[i], i, expected_d[i]);
#else
	abort();
#endif
  }
}
