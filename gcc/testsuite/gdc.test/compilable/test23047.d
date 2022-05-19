/* REQUIRED_ARGS: -defaultlib= -c -O
 */

// https://issues.dlang.org/show_bug.cgi?id=23047
version(D_SIMD):
alias long2 = __vector(long[2]);

long2 _mm_srl_epi64 ()
{
    long2 r = void;
    r[0] = 1;
    return r;
}
