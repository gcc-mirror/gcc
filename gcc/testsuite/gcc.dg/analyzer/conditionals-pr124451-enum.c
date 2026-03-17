/* { dg-additional-options "-std=c99" } */

typedef enum { false = 0, true } BOOL;
#define NULL ((void *)0)

int test_eq_null_inverted ()
{
    char *p = __builtin_malloc(1);
    const BOOL p_is_null = (p == NULL);
    if (!p_is_null) __builtin_free(p);
    return 0;
}

int test_eq_null_compared_against_false ()
{
    char *p = __builtin_malloc(1);
    const BOOL p_is_null = (p == NULL);
    if (p_is_null == false) __builtin_free(p);
    return 0;
}

int test_ne_null ()
{
    char *p = __builtin_malloc(1);
    const BOOL p_is_notnull = (p != NULL);
    if (p_is_notnull) __builtin_free(p);
    return 0;
}

int test_eq_null ()
{
    char *p = __builtin_malloc(1);
    const BOOL p_is_null = (p == NULL);
    if (p_is_null) {} else __builtin_free(p);
    return 0;
}
