/* PR c/98592 - ICE in gimple_canonical_types_compatible_p while formatting
   a MEM_REF
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

void f (int);

void vlaNx3_to_pia1 (int n)
{
  int a[n][3];

  /* The VLA isn't formatted correctly due to PR 98587.  Just verify
     there is no ICE and a warning is issued.  */
  f (((*(int(*)[4])&a[1][2]))[3]);      // { dg-warning "\\\[-Wuninitialized" }
}

void vlaNxN_to_pia1 (int n)
{
  int a[n][n];

  /* Same as above.  */
  f (((*(int(*)[4])&a[1][2]))[3]);      // { dg-warning "\\\[-Wuninitialized" }
}

void vlaNxN_to_pvla4xN (int n)
{
  int a[n][n];

  /* Same as above.  */
  f (((*(int(*)[4][n])&a[1][2]))[3][4]);  // { dg-warning "\\\[-Wuninitialized" }
}

void vlaN_to_pia2 (int n)
{
  int a[n];

  /* Same as above.  */
  f (((*(int(*)[3][4])&a[1]))[2][3]);   // { dg-warning "\\\[-Wuninitialized" }
}

void vlaN_to_pvlaNx4 (int n)
{
  int a[n];

  /* Same as above.  */
  f (((*(int(*)[n][4])&a[1]))[1][3]);   // { dg-warning "\\\[-Wuninitialized" }
}
