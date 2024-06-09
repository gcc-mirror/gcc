/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64 -O3" } */

#define DEF_ATTR_FUNC(ATTR, ID)                      \
void ATTR                                            \
test_##ID (int *a, int *b, int *out, unsigned count) \
{                                                    \
  unsigned i;                                        \
                                                     \
  for (i = 0; i < count; i++)                        \
    out[i] = a[i] + b[i];                            \
}

DEF_ATTR_FUNC (__attribute__((target("arch=+zve32x"))),    1)
DEF_ATTR_FUNC (__attribute__((target("arch=+zve32f"))),    2)
DEF_ATTR_FUNC (__attribute__((target("arch=+zve64x"))),    3)
DEF_ATTR_FUNC (__attribute__((target("arch=+zve64f"))),    4)
DEF_ATTR_FUNC (__attribute__((target("arch=+zve64d"))),    5)
DEF_ATTR_FUNC (__attribute__((target("arch=+v"))),         6)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl64b"))),    7)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl128b"))),   8)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl256b"))),   9)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl512b"))),  10)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl1024b"))), 11)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl2048b"))), 12)
DEF_ATTR_FUNC (__attribute__((target("arch=+zvl4096b"))), 13)
