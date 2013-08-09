/* { dg-do compile } */
/* { dg-require-effective-target arm_neon } */
/* { dg-add-options arm_neon } */
/* { dg-options "-O3" } */

int id;
int
test (const long int *data)
{
  int i, retval;
  retval = id;
  for (i = 0; i < id; i++)
    {
      retval &= (data[i] <= 0);
    }

  return (retval);
}
