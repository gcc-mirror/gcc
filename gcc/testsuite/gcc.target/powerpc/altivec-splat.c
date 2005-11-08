/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-xfail-if "" { "powerpc-*-eabispe*" "powerpc-ibm-aix*" } { "*" } { "" } } */
/* { dg-options "-maltivec -mabi=altivec -O2" } */

/* Testcase by Richard Guenther and Steven Bosscher.
   Check that "easy" AltiVec constants are correctly synthesized
   if they need to be reloaded.  */

typedef __attribute__ ((vector_size (16))) unsigned char v16qi;
typedef __attribute__ ((vector_size (16))) unsigned short v8hi;
typedef __attribute__ ((vector_size (16))) unsigned int v4si;

#define REGLIST								\
  "77",  "78",  "79",  "80",  "81",  "82",  "83",  "84",  "85",  "86",	\
  "87",  "88",  "89",  "90",  "91",  "92",  "93",  "94",  "95",  "96",	\
  "97",  "98",  "99", "100", "101", "102", "103", "104", "105", "106",	\
 "107", "108"


#define TEST(a, result, b)				\
  void a##_##b (int h)					\
  {							\
    volatile a tmp;					\
    while (h-- > 0)					\
      {							\
        asm ("" : : : REGLIST);				\
        tmp = (a) (result) __builtin_altivec_##b (5);	\
      }							\
  }							\
							\
  void a##_##b##_neg (int h)				\
  {							\
    volatile a tmp;					\
    while (h-- > 0)					\
      {							\
        asm ("" : : : REGLIST);				\
        tmp = (a) (result) __builtin_altivec_##b (-5);	\
      }							\
  }

TEST(v16qi, v16qi, vspltisb)
TEST(v16qi, v8hi, vspltish)
TEST(v16qi, v4si, vspltisw)
TEST(v8hi, v16qi, vspltisb)
TEST(v8hi, v8hi, vspltish)
TEST(v8hi, v4si, vspltisw)
TEST(v4si, v16qi, vspltisb)
TEST(v4si, v8hi, vspltish)
TEST(v4si, v4si, vspltisw)
