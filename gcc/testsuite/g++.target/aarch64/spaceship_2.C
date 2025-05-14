// PR117013
/* { dg-do run } */
// { dg-options "-O2 -std=c++20 -save-temps" }

#include <compare>

#ifndef fp_type
#define fp_type float
#endif

#define TEST_SS_IDIOM(ARGA, ARGB, EXPECT)                   \
    if (spaceship_idiom ((ARGA), (ARGB)) != (EXPECT))       \
        __builtin_abort();                                  \

#define TEST_BR_ON_SS(ARGA, ARGB, EXPECT)                   \
    if(branch_on_spaceship ((ARGA), (ARGB)) != (EXPECT))    \
        __builtin_abort();                                  \


#define RUN_TEST(ARGA, ARGB, EXPECT)                        \
    TEST_SS_IDIOM(ARGA, ARGB, EXPECT)                       \
    TEST_BR_ON_SS(ARGA, ARGB, EXPECT)                       \

/*  Test when .SPACESHIP prompts the back end to implement <=> with
    conditional branches (only applies to floating-point operands).  */

[[gnu::noipa]] auto
equiv() { return std::partial_ordering::equivalent; }
[[gnu::noipa]] auto
less() { return std::partial_ordering::less; }
[[gnu::noipa]] auto
greater() { return std::partial_ordering::greater; }
[[gnu::noipa]] auto
unordered() { return std::partial_ordering::unordered; }

auto
spaceship_idiom(fp_type a, fp_type b)
{
    if (a == b)
        return equiv();
    if (a < b)
        return less();
    if (a > b)
        return greater();
    return unordered();
}

auto
branch_on_spaceship(fp_type a, fp_type b)
{
    auto res = a <=> b;
    if (res == 0)
        return equiv();
    else if (res < 0)
        return less();
    else if (res > 0)
        return greater();
    return unordered();
}

int
main()
{
    RUN_TEST (-1.0f, 1.0f, std::partial_ordering::less);
    RUN_TEST (1.0f, -1.0f, std::partial_ordering::greater);
    RUN_TEST (1.0f, 1.0f, std::partial_ordering::equivalent);
    RUN_TEST (1.0f, __builtin_nanf(""), std::partial_ordering::unordered);
    RUN_TEST (__builtin_nanf(""), 1.0f, std::partial_ordering::unordered);
}

/* { dg-final { scan-assembler-not "\tfcmp\t" } } */
/* { dg-final { scan-assembler-times "\tfcmpe\t" 2 } } */