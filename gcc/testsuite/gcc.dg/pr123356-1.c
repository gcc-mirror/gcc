/* { dg-do run } */
/* { dg-options "-O2" } */

typedef struct
{
    long  coeffs;
}
fmpz_poly_struct;
typedef struct
{
} n_poly_struct;
typedef struct
{
    n_poly_struct * coeffs;
    long alloc;
    long length;
} n_bpoly_struct;
typedef struct
{
    fmpz_poly_struct * coeffs;
    long alloc;
    long length;
} fmpz_bpoly_struct;
typedef fmpz_bpoly_struct fmpz_bpoly_t[];

__attribute__((noinline))
fmpz_poly_struct * fmpz_bpoly_swap_(fmpz_bpoly_t B, fmpz_bpoly_t Q)
{
    fmpz_bpoly_struct t = *B;
    *B = *Q;
    *Q = t;
    return B->coeffs;
}

__attribute__((noinline,optimize("no-strict-aliasing")))
fmpz_poly_struct * fmpz_bpoly_swap_2(fmpz_bpoly_t B, fmpz_bpoly_t Q)
{
    fmpz_bpoly_struct t = *B;
    *B = *Q;
    *Q = t;
    return B->coeffs;
}

int main(){
    fmpz_poly_struct B_coeffs = {0}, Q_coeffs = {0};
    fmpz_bpoly_t B = {0};
    fmpz_bpoly_t Q = {0};
    B->coeffs = &B_coeffs;
    Q->coeffs = &Q_coeffs;
    if (fmpz_bpoly_swap_(B, Q) != &Q_coeffs)
	    __builtin_abort();
    B->coeffs = &B_coeffs;
    Q->coeffs = &Q_coeffs;
    if (fmpz_bpoly_swap_2(B, Q) != &Q_coeffs)
	    __builtin_abort();
    return 0;
}

