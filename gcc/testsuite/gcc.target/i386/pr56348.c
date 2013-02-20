/* PR target/56348 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -fPIC -mfpmath=sse -msse2" } */

typedef unsigned int size_t;

extern double fabs (double __x) __attribute__ ((__nothrow__, __leaf__))
  __attribute__ ((__const__));

typedef struct cholmod_sparse_struct
{
  size_t ncol;
  void *p;
} cholmod_sparse;

int cholmod_l_reallocate_sparse (size_t, cholmod_sparse *, void *);

int
cholmod_l_drop (double tol, cholmod_sparse * A)
{
  double aij;
  double *Ax;
  long long *Ap, *Ai, *Anz;
  long long packed, i, j, nrow, ncol, p, pend, nz, values;
  Ap = A->p;
  ncol = A->ncol;
  nz = 0;
  for (j = 0; j < ncol; j++)
    for (; p < pend; p++)
      {
        i = Ai[p];
        aij = Ax[p];
        if (i <= j && (fabs (aij) > tol || ((aij) != (aij))))
          nz++;
      }
  Ap[ncol] = nz;
  cholmod_l_reallocate_sparse (nz, A, 0);
}
