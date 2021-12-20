/* { dg-do compile { target *-*-darwin* } } */
/* { dg-additional-options "-fcommon" } */

/* Test alignment rules which differ for earlier hosts (so we must
   work on the principle that this test will be exercised by self-
   hosted compilers. */

#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1070
#define align_OK (1ul << 28)
#define align_BAD (1ul << 29)
#else
#define align_OK (1ul << 15)
#define align_BAD (1ul << 16)
#endif

/* All non common vars are allowed larger alignment on modern systems.  */
static int xn __attribute__ ((aligned (align_OK)));
static int xi __attribute__ ((aligned (align_OK))) = 5  ;
int gxi __attribute__ ((aligned (align_OK))) = 6 ;

/* test that we detect bad cases.  */
static int yn __attribute__ ((aligned (align_BAD))); /* { dg-error {requested alignment .[0-9]+. exceeds object file maximum} } */
static int yi __attribute__ ((aligned (align_BAD))) = 5;  /* { dg-error {requested alignment .[0-9]+. exceeds object file maximum} } */
int yni __attribute__ ((aligned (align_BAD))) = 6;  /* { dg-error {requested alignment .[0-9]+. exceeds object file maximum} } */
