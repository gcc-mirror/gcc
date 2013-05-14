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

/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 0 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
