/* { dg-options "-O2 -fdump-ipa-profile -mtune=core2" } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } { "*" } { "" } } */

char *buffer1;
char *buffer2;

#define DEFINE_TEST(N) \
__attribute__((noinline)) \
void bzero_test_ ## N (int len) \
{ \
  __builtin_bzero (buffer1, len); \
} \
 \
__attribute__((noinline)) \
void memcpy_test_ ## N (int len) \
{ \
  __builtin_memcpy (buffer1, buffer2, len); \
} \
 \
__attribute__((noinline)) \
void mempcpy_test_ ## N (int len) \
{ \
  __builtin_mempcpy (buffer1, buffer2, len); \
} \
 \
__attribute__((noinline)) \
void memset_test_ ## N (int len) \
{ \
  __builtin_memset (buffer1, 'c', len); \
} \
 \
void test_stringops_ ## N(int len) \
{ \
  bzero_test_ ## N (len); \
  memcpy_test_## N (len); \
  mempcpy_test_ ## N (len); \
  memset_test_ ## N (len); \
} \
 \
void test_stringops_with_values_ ## N (int common, int not_common) \
{ \
  int i; \
  for (i = 0; i < 1000; i++) \
    { \
      if (i > 990) \
	test_stringops_ ## N (not_common); \
      else \
	test_stringops_ ## N (common); \
    } \
}

DEFINE_TEST(0);
DEFINE_TEST(1);
DEFINE_TEST(2);

int main() {
  buffer1 = __builtin_malloc (1000);
  buffer2 = __builtin_malloc (1000);

  test_stringops_with_values_0 (8, 55);
  test_stringops_with_values_1 (55, 55);
  test_stringops_with_values_2 (257, 55);

  return 0;
}

/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 8 stringop transformation on __builtin_bzero" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 55 stringop transformation on __builtin_bzero" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Single value 32 stringop transformation on __builtin_bzero" 0 "profile" } } */

/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 8 stringop transformation on __builtin_memcpy" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 55 stringop transformation on __builtin_memcpy" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Single value 32 stringop transformation on __builtin_memcpy" 0 "profile" } } */

/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 8 stringop transformation on __builtin_mempcpy" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 55 stringop transformation on __builtin_mempcpy" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Single value 32 stringop transformation on __builtin_mempcpy" 0 "profile" } } */

/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 8 stringop transformation on __builtin_memset" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 55 stringop transformation on __builtin_memset" "profile" } } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Single value 32 stringop transformation on __builtin_memset" 0 "profile" } } */
