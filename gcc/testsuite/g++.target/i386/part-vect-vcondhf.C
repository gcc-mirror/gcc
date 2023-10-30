/* PR target/103861 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */
/* { dg-final { scan-assembler-times "vpcmpeqw" 6 } } */
/* { dg-final { scan-assembler-times "vpcmpgtw" 2 } } */
/* { dg-final { scan-assembler-times "vcmpph" 8 } } */
/* { dg-final { scan-assembler-times "vpblendvb" 8 } } */
typedef unsigned short  __attribute__((__vector_size__ (4))) __v2hu;
typedef short __attribute__((__vector_size__ (4))) __v2hi;

typedef unsigned short  __attribute__((__vector_size__ (8))) __v4hu;
typedef short __attribute__((__vector_size__ (8))) __v4hi;

typedef _Float16 __attribute__((__vector_size__ (4))) __v2hf;
typedef _Float16 __attribute__((__vector_size__ (8))) __v4hf;


__v2hu au, bu;
__v2hi as, bs;
__v2hf af, bf;

__v4hu cu, du;
__v4hi cs, ds;
__v4hf cf, df;

__v2hf auf (__v2hu a, __v2hu b) { return (a > b) ? af : bf; }
__v2hf asf (__v2hi a, __v2hi b) { return (a > b) ? af : bf; }
__v2hu afu (__v2hf a, __v2hf b) { return (a > b) ? au : bu; }
__v2hi afs (__v2hf a, __v2hf b) { return (a > b) ? as : bs; }

__v4hf cuf (__v4hu c, __v4hu d) { return (c > d) ? cf : df; }
__v4hf csf (__v4hi c, __v4hi d) { return (c > d) ? cf : df; }
__v4hu cfu (__v4hf c, __v4hf d) { return (c > d) ? cu : du; }
__v4hi cfs (__v4hf c, __v4hf d) { return (c > d) ? cs : ds; }

__v2hf auf_ne (__v2hu a, __v2hu b) { return (a != b) ? af : bf; }
__v2hf asf_ne (__v2hi a, __v2hi b) { return (a != b) ? af : bf; }
__v2hu afu_ne (__v2hf a, __v2hf b) { return (a != b) ? au : bu; }
__v2hi afs_ne (__v2hf a, __v2hf b) { return (a != b) ? as : bs; }

__v4hf cuf_ne (__v4hu c, __v4hu d) { return (c != d) ? cf : df; }
__v4hf csf_ne (__v4hi c, __v4hi d) { return (c != d) ? cf : df; }
__v4hu cfu_ne (__v4hf c, __v4hf d) { return (c != d) ? cu : du; }
__v4hi cfs_ne (__v4hf c, __v4hf d) { return (c != d) ? cs : ds; }
