/* GNU C dependencies:
   Checker support hooks
   ISO C 9x array element initialization
   void-pointer arithmetic */

typedef __SIZE_TYPE__ size_t;

extern void *malloc (size_t);
extern int printf (const char *, ...);

/* This comes from gcc internals.  Should be exported.  */
enum memory_use_mode {MEMORY_USE_BAD = 0, MEMORY_USE_RO = 1,
		      MEMORY_USE_WO = 2, MEMORY_USE_RW = 3,
		      MEMORY_USE_TW = 6, MEMORY_USE_DONT = 99};

enum access_mode {
  ACCESS_NONE = 0, ACCESS_RO = 1, ACCESS_WO = 2, ACCESS_RW = 3
};

#define NOCHECK __attribute__ ((no_check_memory_usage))

void mark_region (const void *, size_t, enum access_mode) NOCHECK;
void setup () NOCHECK;
void test ();
extern int expect_error;

void *c_malloc (size_t) NOCHECK;
void c_free (void *) NOCHECK;
