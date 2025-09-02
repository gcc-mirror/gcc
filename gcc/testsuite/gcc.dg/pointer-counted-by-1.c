/* More testing the correct usage of attribute counted_by for pointer field.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

typedef struct item1 Item1;
typedef union item2 Item2;

struct pointer_array {
  int count1;
  Item1 *array_1 __attribute__ ((counted_by (count1))); 
  Item2 *array_2 __attribute__ ((counted_by (count2))); 
  int count2;
} *pointer_data; 

struct item1 {
  int a;
  float b[];
};

union item2 {
  int c;
  float d[];
};

void foo ()
{
  pointer_data 
    = (struct pointer_array *) __builtin_malloc (sizeof (struct pointer_array));
  pointer_data->array_1 /* { dg-error "attribute is not allowed for a pointer to structure or union with flexible array member" } */ 
    = (Item1 *) __builtin_malloc (sizeof (Item1) + 3 * sizeof (float));
  pointer_data->array_2 /* { dg-error "attribute is not allowed for a pointer to structure or union with flexible array member" } */
    = (Item2 *) __builtin_malloc (sizeof (Item2) + 3 * sizeof (float));
  return;
}
