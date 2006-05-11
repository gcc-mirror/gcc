/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O2 -maltivec" } */
#define REGLIST                                                              \
         "77",  "78",  "79",  "80",  "81",  "82",  "83",  "84",  "85",  "86",\
         "87",  "88",  "89",  "90",  "91",  "92",  "93",  "94",  "95",  "96",\
         "97",  "98",  "99", "100", "101", "102", "103", "104", "105", "106",\
        "107", "108"

typedef __attribute__ ((vector_size (16))) float v4sf;

void
foo (int H)
{
  volatile v4sf tmp;
  while (H-- > 0)
    {
      asm ("" : : : REGLIST);
      tmp = (v4sf) __builtin_altivec_vspltisw (1);
    }
}

