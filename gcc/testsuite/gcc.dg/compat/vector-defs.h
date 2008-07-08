/* This includes all of the vector modes that are recognized by
   c_common_type_for_mode.  */

typedef int __attribute__((mode(QI))) qi;
typedef int __attribute__((mode(HI))) hi;
typedef int __attribute__((mode(SI))) si;
typedef int __attribute__((mode(DI))) di;
typedef float __attribute__((mode(SF))) sf;
#if ((__SIZEOF_DOUBLE__ >= 8) || (__SIZEOF_LONG_DOUBLE__ >= 8))
typedef float __attribute__((mode(DF))) df;
#else
typedef float __attribute__((mode(SF))) df;
#endif

#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 5)

typedef qi __attribute__((vector_size (8))) v8qi;
typedef qi __attribute__((vector_size (16))) v16qi;

typedef hi __attribute__((vector_size (4))) v2hi;
typedef hi __attribute__((vector_size (8))) v4hi;
typedef hi __attribute__((vector_size (16))) v8hi;

typedef si __attribute__((vector_size (8))) v2si;
typedef si __attribute__((vector_size (16))) v4si;

typedef di __attribute__((vector_size (8))) v1di;
typedef di __attribute__((vector_size (16))) v2di;

typedef sf __attribute__((vector_size (8))) v2sf;
typedef sf __attribute__((vector_size (16))) v4sf;
typedef sf __attribute__((vector_size (64))) v16sf;

typedef df __attribute__((vector_size (16))) v2df;

#else

typedef int __attribute__((mode(V8QI))) v8qi;
typedef int __attribute__((mode(V16QI))) v16qi;

typedef int __attribute__((mode(V2HI))) v2hi;
typedef int __attribute__((mode(V4HI))) v4hi;
typedef int __attribute__((mode(V8HI))) v8hi;

typedef int __attribute__((mode(V2SI))) v2si;
typedef int __attribute__((mode(V4SI))) v4si;

typedef int __attribute__((mode(V1DI))) v1di;
typedef int __attribute__((mode(V2DI))) v2di;

typedef float __attribute__((mode(V2SF))) v2sf;
typedef float __attribute__((mode(V4SF))) v4sf;
typedef float __attribute__((mode(V16SF))) v16sf;

typedef float __attribute__((mode(V2DF))) v2df;

#endif

typedef qi __attribute__((vector_size (32))) v32qi;
typedef hi __attribute__((vector_size (32))) v16hi;
typedef si __attribute__((vector_size (32))) v8si;
typedef di __attribute__((vector_size (32))) v4di;
typedef sf __attribute__((vector_size (32))) v8sf;
typedef df __attribute__((vector_size (32))) v4df;

typedef union U32QI { v32qi v; qi a[32]; } u32qi;
typedef union U16HI { v16hi v; hi a[16]; } u16hi;
typedef union U8SI { v8si v; si a[8]; } u8si;
typedef union U4DI { v4di v; di a[4]; } u4di;
typedef union U8SF { v8sf v; sf a[8]; } u8sf;
typedef union U4DF { v4df v; df a[4]; } u4df;

typedef union U8QI { v8qi v; qi a[8]; } u8qi;
typedef union U16QI { v16qi v; qi a[16]; } u16qi;

typedef union U2HI { v2hi v; hi a[2]; } u2hi;
typedef union U4HI { v4hi v; hi a[4]; } u4hi;
typedef union U8HI { v8hi v; hi a[8]; } u8hi;

typedef union U2SI { v2si v; si a[2]; } u2si;
typedef union U4SI { v4si v; si a[4]; } u4si;

typedef union U1DI { v1di v; di a[1]; } u1di;
typedef union U2DI { v2di v; di a[2]; } u2di;

typedef union U2SF { v2sf v; sf a[2]; } u2sf;
typedef union U4SF { v4sf v; sf a[4]; } u4sf;
typedef union U16SF { v16sf v; sf a[16]; } u16sf;

typedef union U2DF { v2df v; df a[2]; } u2df;
