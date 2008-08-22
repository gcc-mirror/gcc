/* { dg-do compile } */

void
f(int NumberOfSideSets, int *ssNumDFperSide, float *ssDF)
{
  int i;
  float *newssDF = __null;
  int *newssNumDF = new int [NumberOfSideSets];
  int ndf, nextDF, numNewDF = 0;
  int ii=0;
  for (i=0;  i<NumberOfSideSets;  i++)  
    numNewDF += newssNumDF[i];
  if (numNewDF > 0)
    newssDF = new float [numNewDF];
  nextDF = 0;
  ndf = ssNumDFperSide[ii];
  for (i=0;  i<ndf; i++)
    newssDF[nextDF++] = ssDF[i];
}

/* { dg-final { cleanup-tree-dump "vect" } } */
