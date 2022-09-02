/* { dg-do compile } */
/* PR/18047  Test that operators have the right precedence.  */
/* by bonzini@gnu.org */

#define test(lower, higher, a, b, c, d) \
  test_(lower, higher, a, b, c, d, __LINE__)

#define test_(lower, higher, a, b, c, d, line)\
   test__(lower, higher, a, b, c, d, line)

/* The first declaration tests that the parentheses are added correctly
   by the expression parser.  The second tests that the two possible
   orderings of precedences do give different results.  */
#define test__(lower, higher, a, b, c, d, line) \
  char test##line[ \
    (a higher b lower c higher d) == \
    ((a higher b) lower (c higher d)) \
    ? 1 : -1]; \
  char doublecheck##line[ \
    (a higher b lower c higher d) == \
    (a higher (b lower c) higher d) \
    ? -1 : 1];

test (||, &&, 1, 1, 0, 0)
test (&&, |, 5, 1, 1, 19)
test (|, ^, 1, 2, 0x2, 1)
test (^, &, 1, 3, 2, 6)
test (&, ==, 1, 3, 2, 0)
test (==, <, 2, 0, 0, 0)
test (<, <<, 2, 3, 6, 8)
test (<<, +, 2, 3, 4, 5)
test (+, *, 2, 6, 9, 13)
