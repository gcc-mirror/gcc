/* This used to ICE on s390 due to displacement overflow 
   when accessing the low-order subword.  */

/* { dg-do compile } */
/* { dg-options "" } */

struct array 
{
  char align[4092];
  long long elem[2] __attribute__ ((__packed__));
};

long long
test (struct array *array, int i)
{
  return array->elem[i];
}

