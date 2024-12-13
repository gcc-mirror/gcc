/* PR c++/97573 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-error=array-compare" } */

#ifndef __cplusplus
# define bool _Bool
#endif

int arr1[5];
int arr2[5];
int arr3[2][2];
int arr4[2][2];

bool
g ()
{
  bool b = arr1 == arr2; /* { dg-warning "comparison between two arrays" } */
  b &= arr1 != arr2; /* { dg-warning "comparison between two arrays" } */
  b &= arr1 > arr2; /* { dg-warning "comparison between two arrays" } */
  b &= arr1 >= arr2; /* { dg-warning "comparison between two arrays" } */
  b &= arr1 < arr2; /* { dg-warning "comparison between two arrays" } */
  b &= arr1 <= arr2; /* { dg-warning "comparison between two arrays" } */
#ifdef __cplusplus
  b &= +arr1 == +arr2;
  b &= +arr1 != +arr2;
  b &= +arr1 > +arr2;
  b &= +arr1 >= +arr2;
  b &= +arr1 < +arr2;
  b &= +arr1 <= +arr2;
#endif
  b &= &arr1[0] == &arr2[0];
  b &= &arr1[0] != &arr2[0];
  b &= &arr1[0] > &arr2[0];
  b &= &arr1[0] >= &arr2[0];
  b &= &arr1[0] < &arr2[0];
  b &= &arr1[0] <= &arr2[0];

  b &= arr3 == arr4; /* { dg-warning "comparison between two arrays" } */

#if defined(__cplusplus) && __cplusplus > 201703L
  auto cmp = arr1 <=> arr2; /* { dg-error "invalid operands" "" { target c++20 } } */
#endif
  return b;
}
