/* Testing the correct usage of attribute counted_by for pointer field.
   and also mixed pointer field and FMA field in the same structure.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int size;
int *x __attribute__ ((counted_by (size))); /* { dg-error "attribute is not allowed for a non-field declaration" } */

struct pointer_array_0 {
  int count;
  int array __attribute__ ((counted_by (count))); /* { dg-error "attribute is not allowed for a non-array or non-pointer field" } */
  int other;
};

int count;
struct pointer_array_1 {
  int other;
  int *array_1 __attribute__ ((counted_by (count))); /* { dg-error "attribute is not a field declaration in the same structure as" } */
  int array_fam[] __attribute__ ((counted_by (count))); /* { dg-error "attribute is not a field declaration in the same structure as" } */
};

struct pointer_array_2 {
  float count1;
  float count2;
  int *array_2 __attribute__ ((counted_by (count1))); /* { dg-error "attribute is not a field declaration with an integer type" } */
  int array_fam[] __attribute__ ((counted_by (count2))); /* { dg-error "attribute is not a field declaration with an integer type" } */
}; 

struct pointer_array_3 {
  int count;
  int *array_3 __attribute__ ((counted_by (count))) __attribute__ ((counted_by (count)));
}; 

struct pointer_array_4 {
  int count1;
  int count2;
  int *array_4 __attribute__ ((counted_by (count1))) __attribute__ ((counted_by (count2))); /* { dg-error "conflicts with previous declaration" } */
  float array_fam[] __attribute__ ((counted_by (count2))) __attribute__ ((counted_by (count1))); /* { dg-error "conflicts with previous declaration" } */
}; 

struct pointer_array_5 {
  _Bool count;
  int *array_5 __attribute__ ((counted_by (count)));
}; 

enum week {Mon, Tue, Wed};
struct pointer_array_6 {
  enum week days;
  int *array_6 __attribute__ ((counted_by (days)));
}; 

/* counted_by is allowed for pointer to void when GNU extension is enabled.  */
struct pointer_array_7 {
  int count;
  void *array_7 __attribute__ ((counted_by (count)));
}; 

struct pointer_array_8 {
  int count;
  int (*fpr)(int,int) __attribute__ ((counted_by (count))); /* { dg-error "attribute is not allowed for a pointer to function" } */
}; 

struct item1 {
  int a;
  float b;
};

union item2 {
  char *a;
  int *b; 
};

typedef struct item3 Item3;
typedef union item4 Item4;

struct item5 {
  int a;
  float b[];
};

/* Incomplete structure and union are allowed.  */
struct pointer_array_9 {
  int count1;
  int count2;
  int count3;
  struct item1 *array_1 __attribute__ ((counted_by (count1)));
  union item2 *array_2 __attribute__ ((counted_by (count2)));
  Item3 *array_3 __attribute__ ((counted_by (count3))); 
  Item4 *array_4 __attribute__ ((counted_by (count4))); 
  int count4;
  int count5;
  /* structure with flexible array member is not allowed.  */
  struct item5 *array_5 __attribute__ ((counted_by (count5))); /* { dg-error "attribute is not allowed for a pointer to structure or union with flexible array member" } */
}; 

struct mixed_array {
  int count1;
  float *array_1 __attribute__ ((counted_by (count1)));
  float *array_2 __attribute__ ((counted_by (count1)));
  int count2;
  long *array_3 __attribute__ ((counted_by (count2)));
  long array_4[] __attribute__ ((counted_by (count2)));
};

struct mixed_array_2 {
  float *array_1 __attribute__ ((counted_by (count1)));
  int count1;
  float *array_2 __attribute__ ((counted_by (count1)));
  long *array_3 __attribute__ ((counted_by (count2)));
  int count2;
  long array_4[] __attribute__ ((counted_by (count2)));
};
