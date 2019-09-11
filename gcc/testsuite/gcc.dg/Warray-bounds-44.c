/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int foo(unsigned int state, unsigned char * p, unsigned int p_len)
{
    static char const pattern[] = "abcd";
    static unsigned const pattern_length = sizeof(pattern) - 1;

    if (p_len == 1) {
        return state;
    }

    if (state < pattern_length &&
        p_len == (pattern_length - state) &&
	(!__builtin_constant_p(p_len) ?
         __builtin_memcmp(p, pattern + state, p_len) :
         ((unsigned char*)p)[6] == ((unsigned char*)pattern + state)[6] /* { dg-bogus "array bounds" } */
        )) {

        return 4;
    }
    return 1;
}
