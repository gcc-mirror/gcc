/* { dg-do compile } */
/* { dg-options "-w" } */

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
void r_##TYPE (TYPE x) { data_##TYPE = x; }		\
void s_##TYPE (void) { r_##TYPE (data_##TYPE); }

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
