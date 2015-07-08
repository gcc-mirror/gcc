/* { dg-do compile { target ia32 } } */
/* { dg-options "-mno-sse -mno-mmx -miamcu" } */

/* AVX512F and AVX512BW modes.  */
typedef unsigned char V64QImode __attribute__((vector_size(64)));
typedef unsigned short V32HImode __attribute__((vector_size(64)));
typedef unsigned int V16SImode __attribute__((vector_size(64)));
typedef unsigned long long V8DImode __attribute__((vector_size(64)));
typedef float V16SFmode __attribute__((vector_size(64)));
typedef double V8DFmode __attribute__((vector_size(64)));

/* AVX and AVX2 modes.  */
typedef unsigned char V32QImode __attribute__((vector_size(32)));
typedef unsigned short V16HImode __attribute__((vector_size(32)));
typedef unsigned int V8SImode __attribute__((vector_size(32)));
typedef unsigned long long V4DImode __attribute__((vector_size(32)));
typedef float V8SFmode __attribute__((vector_size(32)));
typedef double V4DFmode __attribute__((vector_size(32)));

/* SSE1 and SSE2 modes.  */
typedef unsigned char V16QImode __attribute__((vector_size(16)));
typedef unsigned short V8HImode __attribute__((vector_size(16)));
typedef unsigned int V4SImode __attribute__((vector_size(16)));
typedef unsigned long long V2DImode __attribute__((vector_size(16)));
typedef float V4SFmode __attribute__((vector_size(16)));
typedef double V2DFmode __attribute__((vector_size(16)));

/* MMX and 3DNOW modes.  */
typedef unsigned char V8QImode __attribute__((vector_size(8)));
typedef unsigned short V4HImode __attribute__((vector_size(8)));
typedef unsigned int V2SImode __attribute__((vector_size(8)));
typedef float V2SFmode __attribute__((vector_size(8)));

/* Test argument loading and unloading of each.  */
#define TEST(TYPE)					\
extern TYPE data_##TYPE;				\
void p_##TYPE (TYPE x) { data_##TYPE = x; }		\
TYPE r_##TYPE (TYPE x) { return x; }			\
void s_##TYPE (void) { p_##TYPE (data_##TYPE); }

TEST(V64QImode)
TEST(V32HImode)
TEST(V16SImode)
TEST(V8DImode)
TEST(V16SFmode)
TEST(V8DFmode)

TEST(V32QImode)
TEST(V16HImode)
TEST(V8SImode)
TEST(V4DImode)
TEST(V8SFmode)
TEST(V4DFmode)

TEST(V16QImode)
TEST(V8HImode)
TEST(V4SImode)
TEST(V2DImode)
TEST(V4SFmode)
TEST(V2DFmode)

TEST(V8QImode)
TEST(V4HImode)
TEST(V2SImode)
TEST(V2SFmode)
