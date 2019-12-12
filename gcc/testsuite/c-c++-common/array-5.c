/* PR c++/87996 - "size of array is negative" error when SIZE_MAX/2 < sizeof(array) <= SIZE_MAX
   { dg-do compile }
   { dg-options "-ftrack-macro-expansion=0" }  */

#define INT16_MAX __INT16_MAX__
#define UINT16_MAX ((INT16_MAX << 1) + 1)

#define DIFF_MAX __PTRDIFF_MAX__
#define SIZE_MAX __SIZE_MAX__

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __SIZE_TYPE__  size_t;

/* Verify errors for types.  */

typedef char i8a1_d_m1_t[DIFF_MAX - 1];
/* The following should also be diagnosed because the difference between
   &i8a1_dx[0] and &i8a1_dx[sizeof i8a1_dx] cannot be represented.
typedef char i8a1_d_t[DIFF_MAX];
*/

typedef char i8a1_d_p1_t[(size_t)DIFF_MAX + 1];    /* { dg-error "size .\[0-9\]+. of array .i8a1_d_p1_t. exceeds maximum object size .\[0-9\]+.|is too large" } */

typedef char i8a1_s_t[SIZE_MAX];                   /* { dg-error "size .\[0-9\]+. of array .i8a1_s_t. exceeds maximum object size .\[0-9\]+.|is too large" } */

typedef int16_t i16a_s_d2_t[SIZE_MAX / 2];         /* { dg-error "size .\[0-9\]+. of array .i16a_s_d2_t. exceeds maximum object size .\[0-9\]+." } */
typedef int16_t i16a_s_d3_t[SIZE_MAX / 3];         /* { dg-error "size .\[0-9\]+. of array .i16a_s_d3_t. exceeds maximum object size .\[0-9\]+." } */
typedef int16_t i16a_s_d4_m1_t[SIZE_MAX / 4 - 1];
typedef int16_t i16a_s_d4_p1_t[SIZE_MAX / 4 + 1];  /* { dg-error "size .\[0-9\]+. of array .i16a_s_d4_p1_t. exceeds maximum object size .\[0-9\]+." } */

/* The internal computation overflows the message doesn't show the object
   size (but GCC should compute the result and print it anyway).  */
typedef int32_t i32a_s_d2_t[SIZE_MAX / 2];         /* { dg-error "size of array .i32a_s_d2_t. exceeds maximum object size .\[0-9\]+." } */
typedef int32_t i32a_s_d3_t[SIZE_MAX / 3];         /* { dg-error "size of array .i32a_s_d3_t. exceeds maximum object size .\[0-9\]+." } */
typedef int32_t i32a_s_d4_t[SIZE_MAX / 4];         /* { dg-error "size .\[0-9\]+. of array .i32a_s_d4_t. exceeds maximum object size .\[0-9\]+." } */


/* Verify errors for objects.  */

char i8a1_d_m1[DIFF_MAX - 1];
/* The following should also be diagnosed because the difference between
   &i8a1_dx[0] and &i8a1_dx[sizeof i8a1_dx] cannot be represented.
char i8a1_d[DIFF_MAX];
*/

char i8a_d_p1[(size_t)DIFF_MAX + 1];    /* { dg-error "size .\[0-9\]+. of array .i8a_d_p1. exceeds maximum object size .\[0-9\]+.|is too large" } */

char i8a_s[SIZE_MAX];                   /* { dg-error "size .\[0-9\]+. of array .i8a_s. exceeds maximum object size .\[0-9\]+.|is too large" } */

int16_t i16a_s_d2[SIZE_MAX / 2];         /* { dg-error "size .\[0-9\]+. of array .i16a_s_d2. exceeds maximum object size .\[0-9\]+." } */
int16_t i16a_s_d3[SIZE_MAX / 3];         /* { dg-error "size .\[0-9\]+. of array .i16a_s_d3. exceeds maximum object size .\[0-9\]+." } */
int16_t i16a_sz_d4_m1[SIZE_MAX / 4 - 1];
int16_t i16a_sz_d4_p1[SIZE_MAX / 4 + 1];  /* { dg-error "size .\[0-9\]+. of array .i16a_sz_d4_p1. exceeds maximum object size .\[0-9\]+." } */

/* The internal computation overflows the message doesn't show the object
   size (but GCC should compute the result and print it anyway).  */
int32_t i32a_s_d2[SIZE_MAX / 2];         /* { dg-error "size of array .i32a_s_d2. exceeds maximum object size .\[0-9\]+." } */
int32_t i32a_s_d3[SIZE_MAX / 3];         /* { dg-error "size of array .i32a_s_d3. exceeds maximum object size .\[0-9\]+." } */
int32_t i32a_s_d4[SIZE_MAX / 4];         /* { dg-error "size .\[0-9\]+. of array .i32a_s_d4. exceeds maximum object size .\[0-9\]+." } */
