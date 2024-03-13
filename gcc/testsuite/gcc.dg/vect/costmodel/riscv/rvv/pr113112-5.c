/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -mrvv-vector-bits=zvl -fno-schedule-insns -fno-schedule-insns2" } */

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

/* { dg-final { scan-assembler {e32,m2} } } */
/* { dg-final { scan-assembler-not {vs1r} } } */
/* { dg-final { scan-assembler-not {vs2r} } } */
/* { dg-final { scan-assembler-not {vs4r} } } */
/* { dg-final { scan-assembler-not {vs8r} } } */
