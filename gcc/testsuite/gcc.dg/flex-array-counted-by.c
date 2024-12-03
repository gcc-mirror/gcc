/* Testing the correct usage of attribute counted_by.  */   
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target wchar } */

#include <wchar.h>

int size;
int x __attribute ((counted_by (size))); /* { dg-error "attribute is not allowed for a non-field declaration" } */

struct trailing {
  int count;
  int field __attribute ((counted_by (count))); /* { dg-error "attribute is not allowed for a non-array field" } */
};

struct trailing_1 {
  int count;
  int array_1[0] __attribute ((counted_by (count))); /* { dg-error "attribute is not allowed for a non-flexible array member field" } */
};

int count;
struct trailing_array_2 {
  int count;
  int array_2[] __attribute ((counted_by ("count"))); /* { dg-error "argument is not an identifier" } */
};

struct trailing_array_3 {
  int other;
  int array_3[] __attribute ((counted_by (L"count"))); /* { dg-error "argument is not an identifier" } */
};

struct trailing_array_4 {
  int other;
  int array_4[] __attribute ((counted_by (count))); /* { dg-error "attribute is not a field declaration in the same structure as" } */
};

int count;
struct trailing_array_5 {
  float count;
  int array_5[] __attribute ((counted_by (count))); /* { dg-error "attribute is not a field declaration with an integer type" } */
}; 

struct trailing_array_6 {
  int count;
  int array_6[] __attribute ((counted_by (count))) __attribute ((counted_by (count)));
}; 

struct trailing_array_7 {
  int count1;
  int count2;
  int array_7[] __attribute ((counted_by (count1))) __attribute ((counted_by (count2))); /* { dg-error "conflicts with previous declaration" } */
}; 

struct trailing_array_8 {
  _Bool count;
  int array_8[] __attribute ((counted_by (count)));
}; 

enum week {Mon, Tue, Wed};
struct trailing_array_9 {
  enum week days;
  int array_9[] __attribute ((counted_by (days)));
}; 
