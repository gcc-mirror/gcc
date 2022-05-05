/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

#define iterations 10000
#define LEN_1D 32000
#define LEN_2D 256
#define ARRAY_ALIGNMENT 64

#pragma GCC push_options
#pragma GCC optimize "-fno-tree-vectorize"

#include <stdio.h>
#include <stdlib.h>
#if !defined(__APPLE__) && !defined(__DragonFly__)
#include <malloc.h>
#endif
#include <string.h>
#include <math.h>

typedef float real_t;
#define ABS fabsf

int dummy(real_t[LEN_1D], real_t[LEN_1D], real_t[LEN_1D], real_t[LEN_1D],
          real_t[LEN_1D], real_t[LEN_2D][LEN_2D], real_t[LEN_2D][LEN_2D],
          real_t[LEN_2D][LEN_2D], real_t);

__attribute__((aligned(ARRAY_ALIGNMENT)))
real_t flat_2d_array[LEN_2D * LEN_2D];
__attribute__((aligned(ARRAY_ALIGNMENT))) real_t x[LEN_1D];
__attribute__((aligned(ARRAY_ALIGNMENT))) real_t a[LEN_1D], b[LEN_1D],
    c[LEN_1D], d[LEN_1D], e[LEN_1D], aa[LEN_2D][LEN_2D], bb[LEN_2D][LEN_2D],
    cc[LEN_2D][LEN_2D], tt[LEN_2D][LEN_2D];
__attribute__((aligned(ARRAY_ALIGNMENT))) int indx[LEN_1D];

real_t* __restrict__ xx;
real_t* yy;

void set_1d_array(real_t * arr, int length, real_t value, int stride);
void set_2d_array(real_t arr[LEN_2D][LEN_2D], real_t value, int stride);

struct args_t {
    struct timeval t1;
    struct timeval t2;
    void * __restrict__ arg_info;
};

enum {SET1D_RECIP_IDX = -1, SET1D_RECIP_IDX_SQ = -2};

real_t sum1d(real_t arr[LEN_1D]);
real_t sum2d(real_t arr[LEN_2D][LEN_2D]);

real_t sum_x();
real_t sum_a();
real_t sum_b();
real_t sum_c();
real_t sum_e();

real_t sum_half_xx();

real_t sum_a_aa();

real_t sum_aa();
real_t sum_bb();
real_t sum_cc();
real_t sum_xx();

real_t sum_aa_bb();

real_t sum_flat_2d_array();

real_t sum1d(real_t arr[LEN_1D]){
    real_t ret = 0.;
    for (int i = 0; i < LEN_1D; i++)
        ret += arr[i];
    return ret;
}

real_t sum2d(real_t arr[LEN_2D][LEN_2D]){
    real_t sum = 0.;
    for (int i = 0; i < LEN_2D; i++){
        for (int j = 0; j < LEN_2D; j++){
            sum += arr[i][j];
        }
    }

    return sum;
}

real_t sum_x()
{
    return sum1d(x);
}

real_t sum_xx()
{
    return sum1d(xx);
}

real_t sum_a()
{
    return sum1d(a);
}

real_t sum_b()
{
    return sum1d(b);
}

real_t sum_a_aa()
{
    return sum1d(a) + sum2d(aa);
}

real_t sum_c()
{
    return sum1d(c);
}

real_t sum_e()
{
    return sum1d(e);
}

real_t sum_aa()
{
    return sum2d(aa);
}

real_t sum_bb()
{
    return sum2d(bb);
}

real_t sum_aa_bb()
{
    return sum2d(aa) + sum2d(bb);
}

real_t sum_cc()
{
    return sum2d(cc);
}

real_t sum_half_xx()
{
    real_t temp = 00;

    for (int i = 0; i < LEN_1D/2; i++){
        temp += xx[i];
    }

    return temp;
}

real_t sum_flat_2d_array()
{
    real_t sum = 0.;

    for (int i = 0; i < LEN_2D*LEN_2D; i++){
        sum += flat_2d_array[i];
    }

    return sum;
}


void set_1d_array(real_t * arr, int length, real_t value, int stride)
{
    if (stride == SET1D_RECIP_IDX) {
        for (int i = 0; i < length; i++) {
            arr[i] = 1. / (real_t) (i+1);
        }
    } else if (stride == SET1D_RECIP_IDX_SQ) {
        for (int i = 0; i < length; i++) {
            arr[i] = 1. / (real_t) ((i+1) * (i+1));
        }
    } else {
        for (int i = 0; i < length; i += stride) {
            arr[i] = value;
        }
    }
}

void set_2d_array(real_t arr[LEN_2D][LEN_2D], real_t value, int stride)
{
    for (int i = 0; i < LEN_2D; i++) {
        set_1d_array(arr[i], LEN_2D, value, stride);
    }
}

void init(int** ip, real_t* s1, real_t* s2){
#if !defined (__APPLE__) && !defined (_AIX) && !defined(__DragonFly__)
    xx = (real_t*) memalign(ARRAY_ALIGNMENT, LEN_1D*sizeof(real_t));
    *ip = (int *) memalign(ARRAY_ALIGNMENT, LEN_1D*sizeof(real_t));
#else
# if defined (__APPLE__) \
    && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1060
    /* We have no aligned allocator, but malloc is guaranteed to return
       alignment suitable for the largest vector item.  */
    xx = (real_t*) malloc (LEN_1D*sizeof(real_t));
    *ip = (int *) malloc (LEN_1D*sizeof(real_t));
# else
    posix_memalign ((void*)&xx, ARRAY_ALIGNMENT, LEN_1D*sizeof(real_t));
    posix_memalign ((void*)ip, ARRAY_ALIGNMENT, LEN_1D*sizeof(real_t));
# endif
#endif    

    for (int i = 0; i < LEN_1D; i = i+5){
        (*ip)[i]   = (i+4);
        (*ip)[i+1] = (i+2);
        (*ip)[i+2] = (i);
        (*ip)[i+3] = (i+3);
        (*ip)[i+4] = (i+1);
    }

    set_1d_array(a, LEN_1D, 1.,1);
    set_1d_array(b, LEN_1D, 1.,1);
    set_1d_array(c, LEN_1D, 1.,1);
    set_1d_array(d, LEN_1D, 1.,1);
    set_1d_array(e, LEN_1D, 1.,1);
    set_1d_array(x, LEN_1D, 1.,1);
    set_2d_array(aa, 0.,SET1D_RECIP_IDX);
    set_2d_array(bb, 0.,SET1D_RECIP_IDX);
    set_2d_array(cc, 0.,SET1D_RECIP_IDX);

    for (int i = 0; i < LEN_1D; i++) {
        indx[i] = (i+1) % 4+1;
    }

    *s1 = 1.0;
    *s2 = 2.0;
}

int initialise_arrays(const char* name)
{
    real_t any=0.;
    real_t zero=0.;
    real_t half=.5;
    real_t one=1.;
    real_t two=2.;
    real_t small = .000001;
    int unit =1;
    int frac = SET1D_RECIP_IDX;
    int frac2 = SET1D_RECIP_IDX_SQ;

    if    (!strcmp(name, "s000")) {
      for (int i = 0; i < LEN_1D; i++) {
            a[i] = 1+i;
            b[i] = 2+i;
            c[i] = 3+i;
            d[i] = 4+i;
            e[i] = 5+i;
          }
    } else if (!strcmp(name, "s111")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
        set_1d_array(c, LEN_1D, any,frac2);
        set_1d_array(d, LEN_1D, any,frac2);
        set_1d_array(e, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s112")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s113")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s114")) {
        set_2d_array(aa, any,frac);
        set_2d_array(bb, any,frac2);
    } else if (!strcmp(name, "s115")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_2d_array(aa,small,unit);
        set_2d_array(bb,small,unit);
        set_2d_array(cc,small,unit);
    } else if (!strcmp(name, "s116")) {
        set_1d_array(a, LEN_1D, one,unit);
    } else if (!strcmp(name, "s118")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_2d_array(bb,small,unit);
    } else if (!strcmp(name, "s119")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb, any,frac2);
    } else if (!strcmp(name, "s121")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s122")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s123")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s124")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s125")) {
        set_1d_array(flat_2d_array, LEN_2D*LEN_2D,zero,unit);
        set_2d_array(aa, one,unit);
        set_2d_array(bb,half,unit);
        set_2d_array(cc, two,unit);
    } else if (!strcmp(name, "s126")) {
        set_2d_array(bb, one,unit);
        set_1d_array( flat_2d_array, LEN_2D*LEN_2D,any,frac);
        set_2d_array(cc, any,frac);
    } else if (!strcmp(name, "s127")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s128")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, two,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, one,unit);
    } else if (!strcmp(name, "s131")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s132")) {
        set_2d_array(aa, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s141")) {
        set_1d_array( flat_2d_array, LEN_2D*LEN_2D, one,unit);
        set_2d_array(bb, any,frac2);
    } else if (!strcmp(name, "s151")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s152")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D,zero,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s161")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array( &b[0], LEN_1D/2, one,2);
        set_1d_array( &b[1], LEN_1D/2,-one,2);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s162")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s171")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s172")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s173")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s174")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s175")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s176")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s211")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s212")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s221")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s222")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
    } else if (!strcmp(name, "s231")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb, any,frac2);
    } else if (!strcmp(name, "s232")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb,zero,unit);
    } else if (!strcmp(name, "s233")) {
        set_2d_array(aa, any,frac);
        set_2d_array(bb, any,frac);
        set_2d_array(cc, any,frac);
    } else if (!strcmp(name, "s234")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb, any,frac);
        set_2d_array(cc, any,frac);
    } else if (!strcmp(name, "s235")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        set_2d_array(aa, one,unit);
        set_2d_array(bb, any, frac2);
    } else if (!strcmp(name, "s241")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, one,unit);
    } else if (!strcmp(name, "s242")) {
        set_1d_array(a, LEN_1D,small,unit);
        set_1d_array(b, LEN_1D,small,unit);
        set_1d_array(c, LEN_1D,small,unit);
        set_1d_array(d, LEN_1D,small,unit);
    } else if (!strcmp(name, "s243")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s244")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D,small,unit);
        set_1d_array(d, LEN_1D,small,unit);
    } else if (!strcmp(name, "s251")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s252")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
    } else if (!strcmp(name, "s253")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D,small,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s254")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
    } else if (!strcmp(name, "s255")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
    } else if (!strcmp(name, "s256")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, two,frac);
        set_2d_array(aa, two,unit);
        set_2d_array(bb, one,unit);
    } else if (!strcmp(name, "s257")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_2d_array(aa, two,unit);
        set_2d_array(bb, one,unit);
    } else if (!strcmp(name, "s258")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D,zero,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D,zero,unit);
        set_2d_array(aa, any,frac);
    } else if (!strcmp(name, "s261")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
        set_1d_array(c, LEN_1D, any,frac2);
        set_1d_array(d, LEN_1D, one,unit);
    } else if (!strcmp(name, "s271")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s272")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, two,unit);
    } else if (!strcmp(name, "s273")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D,small,unit);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s274")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s275")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb,small,unit);
        set_2d_array(cc,small,unit);
    } else if (!strcmp(name, "s276")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s277")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array( b, LEN_1D/2, one,unit);
        set_1d_array( &b[LEN_1D/2], LEN_1D/2,-one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s278")) {
        set_1d_array( a, LEN_1D/2,-one,unit);
        set_1d_array( &a[LEN_1D/2], LEN_1D/2,one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s279")) {
        set_1d_array( a, LEN_1D/2,-one,unit);
        set_1d_array( &a[LEN_1D/2], LEN_1D/2,one,unit);
//        set_1d_array(a, LEN_1D, -one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s2710")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s2711")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s2712")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s281")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
    } else if (!strcmp(name, "1s281")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, one,unit);
        set_1d_array(e, LEN_1D, one,unit);
        set_1d_array(x, LEN_1D, one,unit);
    } else if (!strcmp(name, "s291")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
    } else if (!strcmp(name, "s292")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
    } else if (!strcmp(name, "s293")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "s2101")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb, any,frac);
        set_2d_array(cc, any,frac);
    } else if (!strcmp(name, "s2102")) {
        set_2d_array(aa,zero,unit);
    } else if (!strcmp(name, "s2111")) {
        set_2d_array(aa, small,unit);
    } else if (!strcmp(name, "s311")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "s312")) {
        set_1d_array(a, LEN_1D,1.000001,unit);
    } else if (!strcmp(name, "s313")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "s314")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "s315")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "s316")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "s317")) {
    } else if (!strcmp(name, "s318")) {
        set_1d_array(a, LEN_1D, any,frac);
        a[LEN_1D-1] = -two;
    } else if (!strcmp(name, "s319")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D,zero,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s3110")) {
        set_2d_array(aa, any,frac);
        aa[LEN_2D-1][LEN_2D-1] = two;
    } else if (!strcmp(name, "s3111")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "s3112")) {
        set_1d_array(a, LEN_1D, any,frac2);
        set_1d_array(b, LEN_1D,zero,unit);
    } else if (!strcmp(name, "s3113")) {
        set_1d_array(a, LEN_1D, any,frac);
        a[LEN_1D-1] = -two;
    } else if (!strcmp(name, "s321")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D,zero,unit);
    } else if (!strcmp(name, "s322")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D,zero,unit);
        set_1d_array(c, LEN_1D,zero,unit);
    } else if (!strcmp(name, "s323")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s331")) {
        set_1d_array(a, LEN_1D, any,frac);
        a[LEN_1D-1] = -one;
    } else if (!strcmp(name, "s332")) {
        set_1d_array(a, LEN_1D, any,frac2);
        a[LEN_1D-1] = two;
    } else if (!strcmp(name, "s341")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "s342")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "s343")) {
        set_2d_array(aa, any,frac);
        set_2d_array(bb, one,unit);
    } else if (!strcmp(name, "s351")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        c[0] = 1.;
    } else if (!strcmp(name, "s352")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "s353")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        c[0] = 1.;
    } else if (!strcmp(name, "s411")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s412")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s413")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s414")) {
        set_2d_array(aa, one,unit);
        set_2d_array(bb, any,frac);
        set_2d_array(cc, any,frac);
    } else if (!strcmp(name, "s415")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        a[LEN_1D-1] = -one;
    } else if (!strcmp(name, "s421")) {
        set_1d_array(a, LEN_1D, any,frac2);
        set_1d_array(flat_2d_array, LEN_1D, one, unit);
    } else if (!strcmp(name, "s422")) {
        set_1d_array(flat_2d_array, LEN_1D,one,unit);
        set_1d_array(a, LEN_1D, any,frac2);
        set_1d_array(flat_2d_array + LEN_1D, LEN_1D, zero, unit);
    } else if (!strcmp(name, "s1421")) {
        set_1d_array(b, LEN_1D, one, unit);
    } else if (!strcmp(name, "s423")) {
        set_1d_array(flat_2d_array, LEN_1D,zero,unit);
        set_1d_array(a, LEN_1D, any,frac2);
        set_1d_array(flat_2d_array + LEN_1D, LEN_1D, one, unit);
    } else if (!strcmp(name, "s424")) {
        set_1d_array(flat_2d_array, LEN_1D,one,unit);
        set_1d_array(a, LEN_1D, any,frac2);
        set_1d_array(flat_2d_array, LEN_1D, zero, unit);
    } else if (!strcmp(name, "s431")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s432")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s441")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(&d[0],             LEN_1D/3  , -one,unit);
        set_1d_array(&d[LEN_1D/3],      LEN_1D/3  , zero,unit);
        set_1d_array(&d[(2*LEN_1D/3)],  LEN_1D/3+1, one,unit);
    } else if (!strcmp(name, "s442")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
    } else if (!strcmp(name, "s443")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s451")) {
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s452")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D,small,unit);
    } else if (!strcmp(name, "s453")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s471")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, one,unit);
        set_1d_array(d, LEN_1D, any,frac);
        set_1d_array(e, LEN_1D, any,frac);
        set_1d_array(x, LEN_1D, zero, unit);
    } else if (!strcmp(name, "s481")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s482")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "s491")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s4112")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "s4113")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac2);
    } else if (!strcmp(name, "s4114")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s4115")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "s4116")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_2d_array(aa, any,frac);
    } else if (!strcmp(name, "s4117")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D, any,frac);
        set_1d_array(d, LEN_1D, any,frac);
    } else if (!strcmp(name, "s4121")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "va")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "vag")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "vas")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "vif")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "vpv")) {
        set_1d_array(a, LEN_1D,zero,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "vtv")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, one,unit);
    } else if (!strcmp(name, "vpvtv")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, any,frac);
    } else if (!strcmp(name, "vpvts")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, any,frac2);
    } else if (!strcmp(name, "vpvpv")) {
        set_1d_array(a, LEN_1D, any,frac2);
        set_1d_array(b, LEN_1D, one,unit);
        set_1d_array(c, LEN_1D,-one,unit);
    } else if (!strcmp(name, "vtvtv")) {
        set_1d_array(a, LEN_1D, one,unit);
        set_1d_array(b, LEN_1D, two,unit);
        set_1d_array(c, LEN_1D,half,unit);
    } else if (!strcmp(name, "vsumr")) {
        set_1d_array(a, LEN_1D, any,frac);
    } else if (!strcmp(name, "vdotr")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
    } else if (!strcmp(name, "vbor")) {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, one,frac);
        set_1d_array(d, LEN_1D, two,frac);
        set_1d_array(e, LEN_1D,half,frac);
        set_2d_array(aa, any,frac);
    } else {
        set_1d_array(a, LEN_1D, any,frac);
        set_1d_array(b, LEN_1D, any,frac);
        set_1d_array(c, LEN_1D, one,frac);
        set_1d_array(d, LEN_1D, two,frac);
        set_1d_array(e, LEN_1D,half,frac);
        set_2d_array(aa, half,frac);
        set_2d_array(bb, one,frac);
        set_2d_array(cc, any,frac);
    }

    return 0;
}

real_t calc_checksum(const char * name)
{
    if (!strcmp(name, "s000")) {
        return sum_a();
    } else if (!strcmp(name, "s111")) {
        return sum_a();
    } else if (!strcmp(name, "s1111")) {
        return sum_a();
    } else if (!strcmp(name, "s112")) {
        return sum_a();
    } else if (!strcmp(name, "s1112")) {
        return sum_a();
    } else if (!strcmp(name, "s113")) {
        return sum_a();
    } else if (!strcmp(name, "s1113")) {
        return sum_a();
    } else if (!strcmp(name, "s114")) {
        return sum_aa();
    } else if (!strcmp(name, "s115")) {
        return sum_a();
    } else if (!strcmp(name, "s1115")) {
        return sum_aa();
    } else if (!strcmp(name, "s116")) {
        return sum_a();
    } else if (!strcmp(name, "s118")) {
        return sum_a();
    } else if (!strcmp(name, "s119")) {
        return sum_aa();
    } else if (!strcmp(name, "s1119")) {
        return sum_aa();
    } else if (!strcmp(name, "s121")) {
        return sum_a();
    } else if (!strcmp(name, "s122")) {
        return sum_a();
    } else if (!strcmp(name, "s123")) {
        return sum_a();
    } else if (!strcmp(name, "s124")) {
        return sum_a();
    } else if (!strcmp(name, "s125")) {
        return sum_flat_2d_array();
    } else if (!strcmp(name, "s126")) {
        return sum_bb();
    } else if (!strcmp(name, "s127")) {
        return sum_a();
    } else if (!strcmp(name, "s128")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s131")) {
        return sum_a();
    } else if (!strcmp(name, "s132")) {
        return sum_aa();
    } else if (!strcmp(name, "s141")) {
        return sum_flat_2d_array();
    } else if (!strcmp(name, "s151")) {
        return sum_a();
    } else if (!strcmp(name, "s152")) {
        return sum_a();
    } else if (!strcmp(name, "s161")) {
        return sum_a() + sum_c();
    } else if (!strcmp(name, "s1161")) {
        return sum_a() + sum_c();
    } else if (!strcmp(name, "s162")) {
        return sum_a();
    } else if (!strcmp(name, "s171")) {
        return sum_a();
    } else if (!strcmp(name, "s172")) {
        return sum_a();
    } else if (!strcmp(name, "s173")) {
        return sum_a();
    } else if (!strcmp(name, "s174")) {
        return sum_a();
    } else if (!strcmp(name, "s175")) {
        return sum_a();
    } else if (!strcmp(name, "s176")) {
        return sum_a();
    } else if (!strcmp(name, "s211")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s212")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s1213")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s221")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s1221")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s222")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s231")) {
        return sum_aa();
    } else if (!strcmp(name, "s232")) {
        return sum_aa();
    } else if (!strcmp(name, "s1232")) {
        return sum_aa();
    } else if (!strcmp(name, "s233")) {
        return sum_aa_bb();
    } else if (!strcmp(name, "s2233")) {
        return sum_aa_bb();
    } else if (!strcmp(name, "s235")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s241")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s242")) {
        return sum_a();
    } else if (!strcmp(name, "s243")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s244")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s1244")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s2244")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s251")) {
        return sum_a();
    } else if (!strcmp(name, "s1251")) {
        return sum_a();
    } else if (!strcmp(name, "s2251")) {
        return sum_a();
    } else if (!strcmp(name, "s3251")) {
        return sum_a();
    } else if (!strcmp(name, "s252")) {
        return sum_a();
    } else if (!strcmp(name, "s253")) {
        return sum_a() + sum_c();
    } else if (!strcmp(name, "s254")) {
        return sum_a();
    } else if (!strcmp(name, "s255")) {
        return sum_a();
    } else if (!strcmp(name, "s256")) {
        return sum_a_aa();
    } else if (!strcmp(name, "s257")) {
        return sum_a_aa();
    } else if (!strcmp(name, "s258")) {
        return sum_b() + sum_e();
    } else if (!strcmp(name, "s261")) {
        return sum_a() + sum_c();
    } else if (!strcmp(name, "s271")) {
        return sum_a();
    } else if (!strcmp(name, "s272")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s273")) {
        return sum_a() + sum_b() + sum_c();
    } else if (!strcmp(name, "s274")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s275")) {
        return sum_aa();
    } else if (!strcmp(name, "s2275")) {
        return sum_aa();
    } else if (!strcmp(name, "s276")) {
        return sum_a();
    } else if (!strcmp(name, "s277")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s278")) {
        return sum_a() + sum_b() + sum_c();
    } else if (!strcmp(name, "s279")) {
        return sum_a() + sum_b() + sum_c();
    } else if (!strcmp(name, "s1279")) {
        return sum_a() + sum_b() + sum_c();
    } else if (!strcmp(name, "s2710")) {
        return sum_a() + sum_b() + sum_c();
    } else if (!strcmp(name, "s2711")) {
        return sum_a();
    } else if (!strcmp(name, "s2712")) {
        return sum_a();
    } else if (!strcmp(name, "s281")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s1281")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s291")) {
        return sum_a();
    } else if (!strcmp(name, "s292")) {
        return sum_a();
    } else if (!strcmp(name, "s293")) {
        return sum_a();
    } else if (!strcmp(name, "s2101")) {
        return sum_aa();
    } else if (!strcmp(name, "s2102")) {
        return sum_aa();
    } else if (!strcmp(name, "s2111")) {
        return sum_aa();
    } else if (!strcmp(name, "s311")) {
        return sum_a();
    } else if (!strcmp(name, "s31111")) {
        return sum_a();
    } else if (!strcmp(name, "s321")) {
        return sum_a();
    } else if (!strcmp(name, "s322")) {
        return sum_a();
    } else if (!strcmp(name, "s323")) {
        return sum_a() + sum_b();
    } else if (!strcmp(name, "s341")) {
        return sum_a();
    } else if (!strcmp(name, "s342")) {
        return sum_a();
    } else if (!strcmp(name, "s343")) {
        return sum_flat_2d_array();
    } else if (!strcmp(name, "s351")) {
        return sum_a();
    } else if (!strcmp(name, "s1351")) {
        return sum_a();
    } else if (!strcmp(name, "s353")) {
        return sum_a();
    } else if (!strcmp(name, "s421")) {
        return sum_xx();
    } else if (!strcmp(name, "s1421")) {
        return sum_half_xx();
    } else if (!strcmp(name, "s422")) {
        return sum_xx();
    } else if (!strcmp(name, "s423")) {
        return sum_flat_2d_array();
    } else if (!strcmp(name, "s424")) {
        return sum_xx();
    } else if (!strcmp(name, "s431")) {
        return sum_a();
    } else if (!strcmp(name, "s441")) {
        return sum_a();
    } else if (!strcmp(name, "s442")) {
        return sum_a();
    } else if (!strcmp(name, "s443")) {
        return sum_a();
    } else if (!strcmp(name, "s451")) {
        return sum_a();
    } else if (!strcmp(name, "s452")) {
        return sum_a();
    } else if (!strcmp(name, "s453")) {
        return sum_a();
    } else if (!strcmp(name, "s471")) {
        return sum_x() + sum_b();
    } else if (!strcmp(name, "s481")) {
        return sum_a();
    } else if (!strcmp(name, "s482")) {
        return sum_a();
    } else if (!strcmp(name, "s491")) {
        return sum_a();
    } else if (!strcmp(name, "s4112")) {
        return sum_a();
    } else if (!strcmp(name, "s4113")) {
        return sum_a();
    } else if (!strcmp(name, "s4114")) {
        return sum_a();
    } else if (!strcmp(name, "s4117")) {
        return sum_a();
    } else if (!strcmp(name, "s4121")) {
        return sum_a();
    } else if (!strcmp(name, "va")) {
        return sum_a();
    } else if (!strcmp(name, "vag")) {
        return sum_a();
    } else if (!strcmp(name, "vas")) {
        return sum_a();
    } else if (!strcmp(name, "vif")) {
        return sum_a();
    } else if (!strcmp(name, "vpv")) {
        return sum_a();
    } else if (!strcmp(name, "vtv")) {
        return sum_a();
    } else if (!strcmp(name, "vpvtv")) {
        return sum_a();
    } else if (!strcmp(name, "vpvts")) {
        return sum_a();
    } else if (!strcmp(name, "vpvpv")) {
        return sum_a();
    } else if (!strcmp(name, "vtvtv")) {
        return sum_a();
    } else if (!strcmp(name, "vsumr")) {
        return sum_a();
    } else if (!strcmp(name, "vbor")) {
        return sum_x();
    } else {
        fprintf(stderr, "Unknown function name passed to calc_checksum: %s\n", name);
        exit(1);
    }
}

real_t get_expected_result(const char * name) 
{
    if (!strcmp(name, "s000")) {
	return 512075584.f;
    } else if (!strcmp(name, "s111")) {
	return 32000.410156f;
    } else if (!strcmp(name, "s1111")) {
	return 13.352669f;
    } else if (!strcmp(name, "s112")) {
	return 81335.929688f;
    } else if (!strcmp(name, "s1112")) {
	return 32009.560547f;
    } else if (!strcmp(name, "s113")) {
	return 32000.642578f;
    } else if (!strcmp(name, "s1113")) {
	return 40010.613281f;
    } else if (!strcmp(name, "s114")) {
	return 919.856323f;
    } else if (!strcmp(name, "s115")) {
	return 31727.289062f;
    } else if (!strcmp(name, "s1115")) {
	return 25487.052734f;
    } else if (!strcmp(name, "s116")) {
	return 32000.f;
    } else if (!strcmp(name, "s118")) {
	return 32353.884766f;
    } else if (!strcmp(name, "s119")) {
	return 86338.984375f;
    } else if (!strcmp(name, "s1119")) {
	return 201466.421875f;
    } else if (!strcmp(name, "s121")) {
	return 32009.027344f;
    } else if (!strcmp(name, "s122")) {
	return 48446.664062f;
    } else if (!strcmp(name, "s123")) {
	return 32003.285156f;
    } else if (!strcmp(name, "s124")) {
	return 32001.642578f;
    } else if (!strcmp(name, "s125")) {
	return 131072.f;
    } else if (!strcmp(name, "s126")) {
	return 66955.132812f;
    } else if (!strcmp(name, "s127")) {
	return 32003.285156f;
    } else if (!strcmp(name, "s128")) {
	return 80000.f;
    } else if (!strcmp(name, "s131")) {
	return 32009.027344f;
    } else if (!strcmp(name, "s132")) {
	return 65538.5625f;
    } else if (!strcmp(name, "s141")) {
	return 3307351.5f;
    } else if (!strcmp(name, "s151")) {
	return 32009.027344f;
    } else if (!strcmp(name, "s152")) {
	return 44020.523438f;
    } else if (!strcmp(name, "s161")) {
	return 64002.054688f;
    } else if (!strcmp(name, "s1161")) {
	return 23.546331f;
    } else if (!strcmp(name, "s162")) {
	return 32009.023438f;
    } else if (!strcmp(name, "s171")) {
	return 48448.019531f;
    } else if (!strcmp(name, "s172")) {
	return 48448.019531f;
    } else if (!strcmp(name, "s173")) {
	return 32001.626953f;
    } else if (!strcmp(name, "s174")) {
	return 32001.626953f;
    } else if (!strcmp(name, "s175")) {
	return 32009.023438f;
    } else if (!strcmp(name, "s176")) {
	return 32000.f;
    } else if (!strcmp(name, "s211")) {
	return 63983.308594f;
    } else if (!strcmp(name, "s212")) {
	return 42008.136719f;
    } else if (!strcmp(name, "s1213")) {
	return 14.450508f;
    } else if (!strcmp(name, "s221")) {
	return 615418176.f;
    } else if (!strcmp(name, "s1221")) {
	return 79623.265625f;
    } else if (!strcmp(name, "s222")) {
	return 32000.f;
    } else if (!strcmp(name, "s231")) {
	return 119107.445312f;
    } else if (!strcmp(name, "s232")) {
	return 65536.f;
    } else if (!strcmp(name, "s1232")) {
	return 2885.801514f;
    } else if (!strcmp(name, "s233")) {
	return 504911.65625f;
    } else if (!strcmp(name, "s2233")) {
	return 337652.8125f;
    } else if (!strcmp(name, "s235")) {
	return 44810.886719f;
    } else if (!strcmp(name, "s241")) {
	return 64000.f;
    } else if (!strcmp(name, "s242")) {
	return 1535966208.f;
    } else if (!strcmp(name, "s243")) {
	return 138653.21875f;
    } else if (!strcmp(name, "s244")) {
	return 64623.015625f;
    } else if (!strcmp(name, "s1244")) {
	return 36.141911f;
    } else if (!strcmp(name, "s2244")) {
	return 32.852161f;
    } else if (!strcmp(name, "s251")) {
	return 32004.367188f;
    } else if (!strcmp(name, "s1251")) {
	return 39967.507812f;
    } else if (!strcmp(name, "s2251")) {
	return 2.635388f;
    } else if (!strcmp(name, "s3251")) {
	return 13.59558f;
    } else if (!strcmp(name, "s252")) {
	return 63999.f;
    } else if (!strcmp(name, "s253")) {
	return 320115936.f;
    } else if (!strcmp(name, "s254")) {
	return 32000.f;
    } else if (!strcmp(name, "s255")) {
	return 31953.501953f;
    } else if (!strcmp(name, "s256")) {
	return 66207.828125f;
    } else if (!strcmp(name, "s257")) {
	return 163072.f;
    } else if (!strcmp(name, "s258")) {
	return 14.65278f;
    } else if (!strcmp(name, "s261")) {
	return 54894.515625f;
    } else if (!strcmp(name, "s271")) {
	return 97793.570312f;
    } else if (!strcmp(name, "s272")) {
	return 64000.f;
    } else if (!strcmp(name, "s273")) {
	return 96311.546875f;
    } else if (!strcmp(name, "s274")) {
	return 320133920.f;
    } else if (!strcmp(name, "s275")) {
	return 65536.f;
    } else if (!strcmp(name, "s2275")) {
	return 1640158.5f;
    } else if (!strcmp(name, "s276")) {
	return 97793.570312f;
    } else if (!strcmp(name, "s277")) {
	return 32000.f;
    } else if (!strcmp(name, "s278")) {
	return 64012.589844f;
    } else if (!strcmp(name, "s279")) {
	return 64014.289062f;
    } else if (!strcmp(name, "s1279")) {
	return 32.852161f;
    } else if (!strcmp(name, "s2710")) {
	return 96003.28125f;
    } else if (!strcmp(name, "s2711")) {
	return 97793.570312f;
    } else if (!strcmp(name, "s2712")) {
	return 97793.570312f;
    } else if (!strcmp(name, "s281")) {
	return 32000.f;
    } else if (!strcmp(name, "s1281")) {
	return INFINITY;
    } else if (!strcmp(name, "s291")) {
	return 32000.f;
    } else if (!strcmp(name, "s292")) {
	return 31953.501953f;
    } else if (!strcmp(name, "s293")) {
	return 31999.998047f;
    } else if (!strcmp(name, "s2101")) {
	return 229657.921875f;
    } else if (!strcmp(name, "s2102")) {
	return 256.f;
    } else if (!strcmp(name, "s2111")) {
	return 34544940.f;
    } else if (!strcmp(name, "s311")) {
	return 10.950721f;
    } else if (!strcmp(name, "s31111")) {
	return 10.950721f;
    } else if (!strcmp(name, "s312")) {
	return 1.030869f;
    } else if (!strcmp(name, "s313")) {
	return 1.644824f;
    } else if (!strcmp(name, "s314")) {
	return 1.f;
    } else if (!strcmp(name, "s315")) {
	return 54857.f;
    } else if (!strcmp(name, "s316")) {
	return 0.000031f;
    } else if (!strcmp(name, "s317")) {
	return 0.f;
    } else if (!strcmp(name, "s318")) {
	return 32002.f;
    } else if (!strcmp(name, "s319")) {
	return 43.802898f;
    } else if (!strcmp(name, "s3110")) {
	return 514.f;
    } else if (!strcmp(name, "s13110")) {
	return 3.f;
    } else if (!strcmp(name, "s3111")) {
	return 10.950725f;
    } else if (!strcmp(name, "s3112")) {
	return 1.644725f;
    } else if (!strcmp(name, "s3113")) {
	return 2.f;
    } else if (!strcmp(name, "s321")) {
	return 32000.f;
    } else if (!strcmp(name, "s322")) {
	return 32000.f;
    } else if (!strcmp(name, "s323")) {
	return 146472.4375f;
    } else if (!strcmp(name, "s331")) {
	return 32000.f;
    } else if (!strcmp(name, "s332")) {
	return -1.f;
    } else if (!strcmp(name, "s341")) {
	return 10.950721f;
    } else if (!strcmp(name, "s342")) {
	return 10.950721f;
    } else if (!strcmp(name, "s343")) {
	return 1567.932129f;
    } else if (!strcmp(name, "s351")) {
	return 2560660224.f;
    } else if (!strcmp(name, "s1351")) {
	return 21.901442f;
    } else if (!strcmp(name, "s352")) {
	return 1.644808f;
    } else if (!strcmp(name, "s353")) {
	return 320084192.f;
    } else if (!strcmp(name, "s421")) {
	return 32009.023438f;
    } else if (!strcmp(name, "s1421")) {
	return 16000.f;
    } else if (!strcmp(name, "s422")) {
	return 3.737715f;
    } else if (!strcmp(name, "s423")) {
	return 64006.683594f;
    } else if (!strcmp(name, "s424")) {
	return 822.364014f;
    } else if (!strcmp(name, "s431")) {
	return 196500.265625f;
    } else if (!strcmp(name, "s441")) {
	return 48448.019531f;
    } else if (!strcmp(name, "s442")) {
	return 40224.117188f;
    } else if (!strcmp(name, "s443")) {
	return 64895.867188f;
    } else if (!strcmp(name, "s451")) {
	return 32007.898438f;
    } else if (!strcmp(name, "s452")) {
	return 32511.939453f;
    } else if (!strcmp(name, "s453")) {
	return 21.901442f;
    } else if (!strcmp(name, "s471")) {
	return 64004.925781f;
    } else if (!strcmp(name, "s481")) {
	return 48448.019531f;
    } else if (!strcmp(name, "s482")) {
	return 48448.019531f;
    } else if (!strcmp(name, "s491")) {
	return 32001.640625f;
    } else if (!strcmp(name, "s4112")) {
	return 141504.875f;
    } else if (!strcmp(name, "s4113")) {
	return 32001.640625f;
    } else if (!strcmp(name, "s4114")) {
	return 32000.f;
    } else if (!strcmp(name, "s4115")) {
	return 1.038636f;
    } else if (!strcmp(name, "s4116")) {
	return 0.753265f;
    } else if (!strcmp(name, "s4117")) {
	return 32002.205078f;
    } else if (!strcmp(name, "s4121")) {
	return 48448.019531f;
    } else if (!strcmp(name, "va")) {
	return 1.644725f;
    } else if (!strcmp(name, "vag")) {
	return 1.644725f;
    } else if (!strcmp(name, "vas")) {
	return 1.644725f;
    } else if (!strcmp(name, "vif")) {
	return 1.644725f;
    } else if (!strcmp(name, "vpv")) {
	return 164487.78125f;
    } else if (!strcmp(name, "vtv")) {
	return 32000.f;
    } else if (!strcmp(name, "vpvtv")) {
	return 97793.570312f;
    } else if (!strcmp(name, "vpvts")) {
	return 17522152701952.f;
    } else if (!strcmp(name, "vpvpv")) {
	return 1.644725f;
    } else if (!strcmp(name, "vtvtv")) {
	return 32000.f;
    } else if (!strcmp(name, "vsumr")) {
	return 10.950725f;
    } else if (!strcmp(name, "vdotr")) {
	return 1.644824f;
    } else if (!strcmp(name, "vbor")) {
	return 31924.046875f;
    } else {
        fprintf(stderr, "Unknown function name passed to expected_result: %s\n", name);
        exit(1);
    }
}

typedef real_t(*test_function_t)(struct args_t *);

static _Bool is_checksum_same(real_t expected, real_t value)
{
  if (expected == INFINITY)
      return value == INFINITY;
  else if(expected == 0.f)
      return value <= 0.01f;
  else {
      real_t fraction = value / expected;
      return 0.99f <= fraction && fraction <= 1.01f;
  }
}

void run(test_function_t vector_func, const char *fname, void * arg_info)
{
    struct args_t func_args = {.arg_info=arg_info};

    double result = vector_func(&func_args);
    double expected_result = get_expected_result(fname);

    if (!is_checksum_same(expected_result, result))
      {
	fprintf (stderr, "value: %f, expected: %f\n", result, expected_result);
	__builtin_abort();
      }
}

int
__attribute__((noipa))
dummy(float a[LEN_1D], float b[LEN_1D], float c[LEN_1D], float d[LEN_1D], float e[LEN_1D], float aa[LEN_2D][LEN_2D], float bb[LEN_2D][LEN_2D], float cc[LEN_2D][LEN_2D], float s){
    // --  called in each loop to make all computations appear required
    return 0;
}

#pragma GCC pop_options
