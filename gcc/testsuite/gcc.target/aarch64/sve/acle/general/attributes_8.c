/* { dg-options "-msve-vector-bits=256 -W -Wall" } */

#include <arm_sve.h>

#define N __ARM_FEATURE_SVE_BITS
#define FIXED_ATTR __attribute__ ((arm_sve_vector_bits (N)))
#define ALIGNED_ATTR __attribute__((aligned(N / 8)))

typedef svint8_t var1;
typedef __SVInt8_t var2;

typedef const var1 const_var1;
typedef const var2 const_var2;

typedef var1 aligned_var1 ALIGNED_ATTR;
typedef var2 aligned_var2 ALIGNED_ATTR;

typedef var1 fixed1 FIXED_ATTR;
typedef var1 fixed1_alias FIXED_ATTR;
typedef var2 fixed2 FIXED_ATTR;

typedef const_var1 const_fixed1 FIXED_ATTR;
typedef const var1 const_fixed1_alias FIXED_ATTR;
typedef const_var2 const_fixed2 FIXED_ATTR;

typedef aligned_var1 aligned_fixed1 FIXED_ATTR;
typedef var1 aligned_fixed1_alias FIXED_ATTR ALIGNED_ATTR;
typedef aligned_var2 aligned_fixed2 FIXED_ATTR;

extern fixed1 extern1;
extern fixed1_alias extern1_alias;
extern fixed2 extern2;

extern const_fixed1 const_extern1;
extern const_fixed1_alias const_extern1_alias;
extern const_fixed2 const_extern2;

fixed1 *ptr1a = &extern1;
fixed1_alias *ptr1b = &extern1;
fixed2 *ptr1c = &extern1;

fixed1 *ptr2a = &extern1_alias;
fixed1_alias *ptr2b = &extern1_alias;
fixed2 *ptr2c = &extern1_alias;

fixed1 *ptr3a = &extern2;
fixed1_alias *ptr3b = &extern2;
fixed2 *ptr3c = &extern2;

fixed1 *ptr4a = &const_extern1; 	// { dg-error {invalid conversion} "c++" { target c++ } }
					// { dg-warning {discards 'const' qualifier} "c" { target c } .-1 }
fixed1_alias *ptr4b = &const_extern1; 	// { dg-error {invalid conversion} "c++" { target c++ } }
					// { dg-warning {discards 'const' qualifier} "c" { target c } .-1 }
fixed2 *ptr4c = &const_extern1; 	// { dg-error {invalid conversion} "c++" { target c++ } }
					// { dg-warning {discards 'const' qualifier} "c" { target c } .-1 }

const fixed1 *ptr5a = &const_extern2;
const fixed1_alias *ptr5b = &const_extern2;
const fixed2 *ptr5c = &const_extern2;

const_fixed1 *const_ptr1a = &extern1;
const_fixed1_alias *const_ptr1b = &extern1;
const_fixed2 *const_ptr1c = &extern1;

const_fixed1 *const_ptr2a = &extern1_alias;
const_fixed1_alias *const_ptr2b = &extern1_alias;
const_fixed2 *const_ptr2c = &extern1_alias;

const_fixed1 *const_ptr3a = &extern2;
const_fixed1_alias *const_ptr3b = &extern2;
const_fixed2 *const_ptr3c = &extern2;

const_fixed1 *const_ptr4a = &const_extern1;
const_fixed1_alias *const_ptr4b = &const_extern1;
const_fixed2 *const_ptr4c = &const_extern1;

const_fixed1 *const_ptr5a = &const_extern1_alias;
const_fixed1_alias *const_ptr5b = &const_extern1_alias;
const_fixed2 *const_ptr5c = &const_extern1_alias;

const_fixed1 *const_ptr6a = &const_extern2;
const_fixed1_alias *const_ptr6b = &const_extern2;
const_fixed2 *const_ptr6c = &const_extern2;

struct normal1 { int x; fixed1 y; };
struct normal1_alias { int x; fixed1_alias y; };
struct normal2 { int x; fixed2 y; };

struct aligned1 { int x; aligned_fixed1 y; };
struct aligned1_alias { int x; aligned_fixed1_alias y; };
struct aligned2 { int x; aligned_fixed2 y; };

#define ASSERT(NAME, TEST) typedef int NAME[(TEST) ? 1 : -1]

ASSERT (check_normal1, sizeof (struct normal1) == N / 8 + 16);
ASSERT (check_normal1_alias, sizeof (struct normal1_alias) == N / 8 + 16);
ASSERT (check_normal2, sizeof (struct normal2) == N / 8 + 16);

ASSERT (check_aligned1, sizeof (struct aligned1) == N / 4);
ASSERT (check_aligned1_alias, sizeof (struct aligned1_alias) == N / 4);
ASSERT (check_aligned2, sizeof (struct aligned2) == N / 4);
