/* PR c/102989 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=gnu23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 575
#define OVFP
#include "../../c-c++-common/torture/builtin-arith-overflow.h"

#ifdef __SIZEOF_INT128__
#define WTYPE __int128
#else
#define WTYPE long long int
#endif

#define TESTS \
T (100, signed _BitInt(38), signed char, unsigned _BitInt(162), -1wb, 0, -1wb, add, 1) \
T (101, unsigned char, unsigned _BitInt(38), unsigned _BitInt(162), 5, 5wb, 10wb, add, 0) \
T (102, signed _BitInt(38), unsigned short, unsigned _BitInt(162), 5wb, 5, 0wb, sub, 0) \
T (103, signed _BitInt(38), unsigned _BitInt(72), unsigned _BitInt(162), 5wb, 6wb, -1wb, sub, 1) \
T (104, signed _BitInt(38), signed _BitInt(38), unsigned _BitInt(162), -1wb, -1wb, 1wb, mul, 0) \
T (105, unsigned _BitInt(38), signed _BitInt(38), unsigned _BitInt(162), 17wb, -2wb, -34wb, mul, 1) \
T (106, unsigned _BitInt(162), signed _BitInt(162), signed _BitInt(38), 5wb, -2wb, -10wb, mul, 0) \
T (107, signed _BitInt(321), signed _BitInt(321), unsigned _BitInt(38), -3wb, 5wb, 2wb, add, 0) \
T (108, signed _BitInt(321), int, unsigned _BitInt(38), -5wb, 3, -2wb, add, 1) \
T (109, int, _BitInt(162), unsigned _BitInt(38), -3, 5wb, 2wb, add, 0) \
T (110, unsigned _BitInt(38), unsigned _BitInt(38), unsigned _BitInt(162), 137438953471wb - 1, (unsigned _BitInt(38)) 137438953471wb + 4, -5wb, sub, 1) \
T (111, signed _BitInt(315), signed _BitInt(315), unsigned, -1wb, 0wb, -1, add, 1) \
T (112, signed _BitInt(315), signed _BitInt(315), unsigned, 5wb, 42wb, 47, add, 0) \
T (113, signed _BitInt(315), signed _BitInt(315), unsigned WTYPE, -1wb, 0wb, -1, add, 1) \
T (114, signed _BitInt(315), signed _BitInt(315), unsigned WTYPE, 15wb, 5wb, 20, add, 0)
T (115, signed _BitInt(315), signed _BitInt(315), unsigned _BitInt(135), -1wb, 0wb, -1wb, add, 1) \
T (116, signed _BitInt(315), signed _BitInt(315), unsigned _BitInt(135), 5wb, 125wb, 130wb, add, 0) \
T (117, signed _BitInt(275), signed _BitInt(275), unsigned _BitInt(125), -1wb, 0wb, -1wb, add, 1) \
T (118, signed _BitInt(275), signed _BitInt(275), unsigned _BitInt(125), 5wb, 125wb, 130wb, add, 0) \
T (119, signed _BitInt(415), signed _BitInt(415), signed _BitInt(178), 95780971304118053647396689196894323976171195136475135wb, 95780971304118053647396689196894323976171195136475137wb, -191561942608236107294793378393788647952342390272950271wb - 1, add, 1) \
T (120, signed _BitInt(415), signed _BitInt(415), signed _BitInt(178), 95780971304118053647396689196894323976171195136475135wb, 95780971304118053647396689196894323976171195136475136wb, 191561942608236107294793378393788647952342390272950271wb, add, 0) \
T (121, signed _BitInt(439), signed _BitInt(439), signed _BitInt(192), 1569275433846670190958947355801916604025588861116008628223wb, 1569275433846670190958947355801916604025588861116008628225wb, -3138550867693340381917894711603833208051177722232017256447wb - 1, add, 1) \
T (122, signed _BitInt(439), signed _BitInt(439), signed _BitInt(192), 1569275433846670190958947355801916604025588861116008628223wb, 1569275433846670190958947355801916604025588861116008628224wb, 3138550867693340381917894711603833208051177722232017256447wb, add, 0) \
T (123, signed _BitInt(575), signed _BitInt(575), signed _BitInt(193), 3138550867693340381917894711603833208051177722232017256447wb, 3138550867693340381917894711603833208051177722232017256449wb, -6277101735386680763835789423207666416102355444464034512895wb - 1, add, 1) \
T (124, signed _BitInt(575), signed _BitInt(575), signed _BitInt(193), 3138550867693340381917894711603833208051177722232017256447wb, 3138550867693340381917894711603833208051177722232017256448wb, 6277101735386680763835789423207666416102355444464034512895wb, add, 0)

TESTS

#undef T
#define T(n, t1, t2, tr, v1, v2, vr, b, o) t##n##b ();
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 575
  TESTS
#endif
  return 0;
}
