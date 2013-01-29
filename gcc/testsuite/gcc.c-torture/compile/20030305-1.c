/* PR c/9799 */
/* Verify that GCC doesn't crash on excess elements
   in initializer for a flexible array member.  */

typedef struct {
    int aaa;
} s1_t;

typedef struct {
    int bbb;
    s1_t s1_array[];
} s2_t;

static s2_t s2_array[]= {
    { 1, 4 },	/* { dg-error "(initialization of flexible array member|near)" } */
    { 2, 5 },	/* { dg-error "(initialization of flexible array member|near)" } */
    { 3, 6 }	/* { dg-error "(initialization of flexible array member|near)" } */
};
