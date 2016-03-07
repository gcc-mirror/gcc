/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

typedef struct {
  int iatom[3];
  int blocknr;
} t_sortblock;

#define DIM 3

void foo (int ncons, t_sortblock *sb, int *iatom)
{
 int i, m;

 for(i=0; (i<ncons); i++,iatom+=3)
   for(m=0; (m<DIM); m++)
     iatom[m]=sb[i].iatom[m];
}

/* The testcase was originally added for correctness reasons but now we
   can vectorize it correctly if the target supports the permutations
   required.  */

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" { target { ! vect_perm } } } } */
