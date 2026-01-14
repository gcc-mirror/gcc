/* { dg-do compile } */
/* { dg-additional-options "-O3 -mavx2 -mno-avx512f" } */

typedef struct {
   double real;
   double imag;
} complex;

typedef struct { complex e[3][3]; } su3_matrix;

void mult_su3_na( su3_matrix *a, su3_matrix *b, su3_matrix *c ){
int i,j;
register double t,ar,ai,br,bi,cr,ci;
    for(i=0;i<3;i++)
      for(j=0;j<3;j++){

        ar=a->e[i][0].real; ai=a->e[i][0].imag;
        br=b->e[j][0].real; bi=b->e[j][0].imag;
        cr=ar*br; t=ai*bi; cr += t;
        ci=ai*br; t=ar*bi; ci -= t;

        ar=a->e[i][1].real; ai=a->e[i][1].imag;
        br=b->e[j][1].real; bi=b->e[j][1].imag;
        t=ar*br; cr += t; t=ai*bi; cr += t;
        t=ar*bi; ci -= t; t=ai*br; ci += t;

        ar=a->e[i][2].real; ai=a->e[i][2].imag;
        br=b->e[j][2].real; bi=b->e[j][2].imag;
        t=ar*br; cr += t; t=ai*bi; cr += t;
        t=ar*bi; ci -= t; t=ai*br; ci += t;

        c->e[i][j].real=cr;
        c->e[i][j].imag=ci;
    }
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized using 32" "vect" } } */
/* { dg-final { scan-tree-dump "optimized: epilogue loop vectorized using 16 byte vectors and unroll factor 1" "vect" } } */
