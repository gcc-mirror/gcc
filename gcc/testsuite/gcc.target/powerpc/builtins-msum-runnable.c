/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#endif

void abort (void);

int
main()
{
  vector __uint128_t arg_uint128, result_uint128, expected_uint128;
  vector __int128_t arg_int128, result_int128, expected_int128;

  arg_uint128[0] = 0x1627384950617243;
  arg_uint128[0] = arg_uint128[0] << 64;
  arg_uint128[0] |= 0x9405182930415263;
  expected_uint128[0] = 0x1627384950617243;
  expected_uint128[0] = expected_uint128[0] << 64;
  expected_uint128[0] |= 0xb6b07e42a570e5fe;
  vector unsigned long long arg_vull2 = {0x12345678,0x44445555};
  vector unsigned long long arg_vull3 = {0x6789abcd,0x66667777};
  result_uint128 = vec_msum (arg_vull2, arg_vull3, arg_uint128);

  if (result_uint128[0] != expected_uint128[0])
    {
#ifdef DEBUG
       printf("result_uint128[0] doesn't match expected_u128[0]\n");
       printf("arg_vull2  %llx %llx \n",  arg_vull2[0], arg_vull2[1]);
       printf("arg_vull3  %llx %llx \n",  arg_vull3[0], arg_vull3[1]);
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
  expected_int128[0] = 0x1627384950617283;
  expected_int128[0] = expected_int128[0] << 64;
  expected_int128[0] |= 0xd99f35969c11cbfa;
  vector signed long long arg_vll2 = { 0x567890ab, 0x1233456 };
  vector signed long long arg_vll3 = { 0xcdef0123, 0x9873451 };
  result_int128 = vec_msum (arg_vll2, arg_vll3, arg_int128);

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
}

