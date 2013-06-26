/* { dg-options "-fcilkplus" } */

int main (void)
{
  extern int func (int);
  int array[10][10], array2[10];
  int argc = 1;
  array2[array[:][:]] = 5; /* { dg-error "rank of the array's index is greater than 1" } */

  array2[array[:][:]] = 5; /* { dg-error "rank of the array's index is greater than 1" } */
  func (array2[array[:][:]]); /* { dg-error "rank of the array's index is greater than 1" } */
  func (array2[array[argc:func(5)][0:10:2]]); /* { dg-error "rank of the array's index is greater than 1" } */
  
  array[array2[:]][array2[:]] = 5; /* This is OK.  */
  array[array2[:]][array2[:]] = array2[array[:][:]]; /* { dg-error "rank of the array's index is greater than 1" }  */
  array[array2[:]][array2[:]] = array2[array[0:10:1][:]]; /* { dg-error "rank of the array's index is greater than 1" }  */
  array[array2[:]][array2[:]] = array2[array[:][argc:func (argc)]]; /* { dg-error "rank of the array's index is greater than 1" }  */
  return 0;
}
