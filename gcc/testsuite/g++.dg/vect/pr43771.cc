/* { dg-do compile } */

void KWayNodeRefine__(int nparts, int *gpwgts, int *badminpwgt, int
*badmaxpwgt)
{
   int i;

   for (i=0; i<nparts; i+=2) {
       badminpwgt[i] = badminpwgt[i+1] = gpwgts[i]+gpwgts[i+1];
       badmaxpwgt[i] = badmaxpwgt[i+1] = gpwgts[i]+gpwgts[i+1];
   }
}

/* { dg-final { cleanup-tree-dump "vect" } } */
