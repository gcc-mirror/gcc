/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  int array[10], array2[10][10];
  int x, ii, jj ;

  switch (array[:]) {  /* { dg-error "cannot be used as a condition for switch statement" } */
  case 1:
    x = 5;
    break;
  case 2:
    x = 2;
    break;
  default:
    x = 9;
  }

  switch (array2[:][:]) { /* { dg-error "cannot be used as a condition for switch statement" } */
  case 1:
    x = 5;
    break;
  case 2:
    x = 2;
    break;
  default:
    x = 9;
  }

  switch (array[:] + x) { /* { dg-error "cannot be used as a condition for switch statement" } */
  case 1:
    x = 5;
    break;
  case 2:
    x = 2;
    break;
  default:
    x = 9;
  }
  
  switch (array2[:][1:x:4] + x) { /* { dg-error "cannot be used as a condition for switch statement" } */
  case 1:
    x = 5;
    break;
  case 2:
    x = 2;
    break;
  default:
    x = 9;
  }

  for (ii = 0; ii < array[:]; ii++) /* { dg-error "cannot be used in a condition for a for-loop" } */
    {
      x = 2;
    }

  for (ii = 0; ii < array2[:][:]; ii++) /* { dg-error "cannot be used in a condition for a for-loop" } */
    {
      x = 3;
    }

  for (; array2[:][:] < 2;) /* { dg-error "cannot be used in a condition for a for-loop" } */
    x = 4;


  while (array2[:][:]) /* { dg-error "cannot be used as a condition for while statement" } */
    x = 3;

  while (array[1:1:1]) /* { dg-error "cannot be used as a condition for while statement" } */
    x = 1;

  while (ii != array2[1:x:3][1:2:1]) /* { dg-error "cannot be used as a condition for while statement"  } */
    x = 2;

  do {
    x = 3;
  } while (ii != array2[:][:]); /* { dg-error "cannot be used as a condition for a do-while statement" } */

  do {
    x = 2;
  } while (ii != (x + array2[:][1:x:2]) + 2); /* { dg-error "cannot be used as a condition for a do-while statement" } */
  
  do { 
    x += 3;
    if (x == 5)
      return array2[:][:]; /* { dg-error "array notation expression cannot be used as a return value" } */
  } while (ii != 0);

  for (ii = 0;  ii < 10; ii++)
    if (ii % 2)
      return array[1:x:ii]; /* { dg-error "array notation expression cannot be used as a return value" } */

  for (ii = 0; ii < x; ii++)
    if (ii)
      return array2[:][:]; /* { dg-error "array notation expression cannot be used as a return value" } */

  for (array[:] = 0; ii < x; ii++) /* This should be OK.  */
    x= 2;

  for (ii = 0; ii < 10; array[:]++) /* This is OK.  */
    x = 5;

  for (jj = 0; jj < 10; array2[:][:]++) /* This is OK.  */
    x = 3;

  for (jj = 0; jj < 10; array2[:][1:x:4]++, jj++) /* This is OK.  */
    x = 3;
  
  return x;
}
 
