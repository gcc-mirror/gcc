/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-forwprop" } */

int zy, h4;

void
r8 (long int mu, int *jr, int *fi, short int dv)
{
  do
    {
      int tx;

      tx = !!h4 ? (zy + h4) : 1;
      mu = tx;
      *jr = (((unsigned char) mu > (254 >> dv)) ? 0 : (unsigned char) tx) + *fi;
    } while (*jr == 0);

  r8 (mu, jr, fi, 1);
}

