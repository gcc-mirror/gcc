// { dg-do assemble  }
// prms-id: 12475

#include <limits.h>

#if LONG_MAX == 2147483647
#define TEST 2147483648
#elif LONG_MAX == 9223372036854775807
#define TEST 9223372036854775808
#else
#error "Unsupported test -- add new constants."
#endif

enum huh { start =-TEST, next };	// { dg-warning "" } 
