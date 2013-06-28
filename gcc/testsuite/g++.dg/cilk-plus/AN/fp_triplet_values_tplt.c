/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

float q;

void func (int *x)
{
  *x = 5;
}
template <class T> int main2 (T x, T y, T z);

int main (void)
{
  main2 <float> (1.5, 2.3, 3.443);
  main2 <double> (1.34393, 2.38383, 4.38383);
  return 0;
}
template <class T> 
int main2 (T x, T y, T z)
{
  int array[10], array2[10];
  array2[:] = array[x:2]; /* { dg-error "start-index of array notation triplet is not an integer" } */
  array2[:] = array[1:y]; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[1:2:z] = array[:]; /* { dg-error "stride of array notation triplet is not an integer" } */
  func (&array2[1:x:3]); /* { dg-error "length of array notation triplet is not an integer" } */
  array2[y:9]++; /* { dg-error "start-index of array notation triplet is not an integer" } */
  array2[1:x]++; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[1:9:x]++; /* { dg-error "stride of array notation triplet is not an integer" } */
  
  ++array2[1:q:3]; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[:] = array[q:1:3]; /* { dg-error "start-index of array notation triplet is not an integer" } */
  array2[:] = array[1:q:3]; /* { dg-error "length of array notation triplet is not an integer" } */
  array2[:] = array[1:3:q]; /* { dg-error "stride of array notation triplet is not an integer" } */
  func (&array2[1:q:3]); /* { dg-error "length of array notation triplet is not an integer" } */
  return 0;
} 
