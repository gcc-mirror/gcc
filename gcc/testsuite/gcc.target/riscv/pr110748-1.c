/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-march=rv64g -mabi=lp64d -O2" } */


void zd(double *d) { *d = 0.0;  }
void zf(float *f)  { *f = 0.0;  }

/* { dg-final { scan-assembler-not "\tfmv\\.d\\.x\t" } } */
/* { dg-final { scan-assembler-not "\tfmv\\.s\\.x\t" } } */
