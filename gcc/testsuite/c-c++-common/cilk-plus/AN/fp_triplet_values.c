/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

float q;

void func (int *x)
{
  *x = 5;
}

int main (void)
{
  int array[10], array2[10];
  array2[:] = array[1.5:2]; /* { dg-error "start-index of array notation triplet is not an integer" } */
  array2[:] = array[1:2.32333333333]; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[1:2:1.5] = array[:]; /* { dg-error "stride of array notation triplet is not an integer" } */
  func (&array2[1:2.34:3]); /* { dg-error "length of array notation triplet is not an integer" } */
  array2[1.43:9]++; /* { dg-error "start-index of array notation triplet is not an integer" } */
  array2[1:9.3]++; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[1:9:0.3]++; /* { dg-error "stride of array notation triplet is not an integer" } */
  
  ++array2[1:q:3]; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[:] = array[q:1:3]; /* { dg-error "start-index of array notation triplet is not an integer" } */
  array2[:] = array[1:q:3]; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[:] = array[1:3:q]; /* { dg-error "stride of array notation triplet is not an integer" } */
  func (&array2[1:q:3]); /* { dg-error "length of array notation triplet is not an integer" } */
  return 0;
} 
