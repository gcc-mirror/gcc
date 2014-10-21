/* { dg-options "-O3 -fgraphite-identity" } */

typedef struct sv TEST_SV;
typedef struct av TEST_AV;
typedef struct magic TEST_MAGIC;
typedef struct xpvav TEST_XPVAV;
struct sv 
{
    void* sv_any;
};
struct av 
{
    TEST_XPVAV* sv_any;
};
struct xpvav 
{
    char* xav_array;
    long int xav_fill;
    long int xav_max;
};
struct magic {
    TEST_SV* mg_obj;
};
extern TEST_SV PL_sv_undef;
void
Perl_av_fill( register TEST_AV *av, int fill)
{
    TEST_MAGIC *mg;
    int key = ((TEST_XPVAV*) (av)->sv_any)->xav_fill;
    TEST_SV** ary = ((TEST_SV**)((TEST_XPVAV*) (av)->sv_any)->xav_array);
    while (key < fill)
          ary[++key] = &PL_sv_undef;
}
