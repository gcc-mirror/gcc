/* { dg-do compile } */

/* Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell 15 Jul 2003 <nathan@codesourcery.com> */

/* you should not be able to pack a typedef to a struct, only the
   underlying struct can be packed.  */

/* ok */
struct u1
{
  char field1;
  short field2;
  int field3;
};

/* ok */
typedef struct p1 {
   char  field1;
   short field2;
   int field3;
} __attribute__ ((packed)) p1_t1;

/* ok */
typedef struct __attribute__ ((packed)) p2 {
   char  field1;
   short field2;
   int field3;
} p2_t1;

int ary1[sizeof (struct p1) == sizeof (p1_t1) ? 1 : -1];
int ary2[sizeof (struct p2) == sizeof (p2_t1) ? 1 : -1];
int ary3[sizeof (struct p1) == sizeof (struct p2) ? 1 : -1];

/* not ok */
typedef struct u1 __attribute__ ((packed)) u1_t1; /* { dg-warning "attribute ignored" "" }*/
typedef struct u1 u1_t2 __attribute__ ((packed)); /* { dg-warning "attribute ignored" "" }*/

typedef struct p3 {
   char  field1;
   short field2;
   int field3;
} p3_t1 __attribute__ ((packed)); /* { dg-warning "attribute ignored" "" }*/

