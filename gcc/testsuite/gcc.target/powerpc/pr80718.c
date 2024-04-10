/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 -ffast-math" } */

/* Taken from the Spec 2006 milc brenchmark.  Ultimately, GCC wants to generate
   a DF splat from offsettable memory.  The register allocator decided it was
   better to do the load in the GPR registers and do a move direct, rather than
   doing a load in the VSX register sets.  */

typedef struct
{
  double real;
  double imag;
} complex;

typedef struct
{
  double real;
  double imag;
} double_complex;

complex cmplx (double x, double y);
complex cadd (complex * a, complex * b);
complex cmul (complex * a, complex * b);
complex csub (complex * a, complex * b);
complex cdiv (complex * a, complex * b);
complex conjg (complex * a);
complex ce_itheta (double theta);

double_complex dcmplx (double x, double y);
double_complex dcadd (double_complex * a, double_complex * b);
double_complex dcmul (double_complex * a, double_complex * b);
double_complex dcsub (double_complex * a, double_complex * b);
double_complex dcdiv (double_complex * a, double_complex * b);
double_complex dconjg (double_complex * a);
double_complex dcexp (double_complex * a);
double_complex dclog (double_complex * a);
double_complex dcsqrt (double_complex * z);
double_complex dce_itheta (double theta);

typedef struct
{
  unsigned long r0, r1, r2, r3, r4, r5, r6;
  unsigned long multiplier, addend, ic_state;
  double scale;
} double_prn;

double myrand (double_prn * prn_pt);

typedef struct
{
  complex e[3][3];
} su3_matrix;

typedef struct
{
  complex c[3];
} su3_vector;

typedef struct
{
  complex m01, m02, m12;
  double m00im, m11im, m22im;
  double space;
} anti_hermitmat;

typedef struct
{
  complex e[2][2];
} su2_matrix;
typedef struct
{
  su3_vector d[4];
} wilson_vector;
typedef struct
{
  su3_vector h[2];
} half_wilson_vector;
typedef struct
{
  wilson_vector c[3];
} color_wilson_vector;
typedef struct
{
  wilson_vector d[4];
} spin_wilson_vector;
typedef struct
{
  color_wilson_vector d[4];
} wilson_matrix;
typedef struct
{
  spin_wilson_vector c[3];
} wilson_propagator;

void mult_su3_nn (su3_matrix * a, su3_matrix * b, su3_matrix * c);
void mult_su3_na (su3_matrix * a, su3_matrix * b, su3_matrix * c);
void mult_su3_an (su3_matrix * a, su3_matrix * b, su3_matrix * c);
double realtrace_su3 (su3_matrix * a, su3_matrix * b);
complex trace_su3 (su3_matrix * a);
complex complextrace_su3 (su3_matrix * a, su3_matrix * b);
complex det_su3 (su3_matrix * a);
void add_su3_matrix (su3_matrix * a, su3_matrix * b, su3_matrix * c);
void sub_su3_matrix (su3_matrix * a, su3_matrix * b, su3_matrix * c);
void scalar_mult_su3_matrix (su3_matrix * src, double scalar,
			     su3_matrix * dest);
void scalar_mult_add_su3_matrix (su3_matrix * src1, su3_matrix * src2,
				 double scalar, su3_matrix * dest);
void scalar_mult_sub_su3_matrix (su3_matrix * src1, su3_matrix * src2,
				 double scalar, su3_matrix * dest);
void c_scalar_mult_su3mat (su3_matrix * src, complex * scalar,
			   su3_matrix * dest);
void c_scalar_mult_add_su3mat (su3_matrix * src1, su3_matrix * src2,
			       complex * scalar, su3_matrix * dest);
void c_scalar_mult_sub_su3mat (su3_matrix * src1, su3_matrix * src2,
			       complex * scalar, su3_matrix * dest);
void su3_adjoint (su3_matrix * a, su3_matrix * b);
void make_anti_hermitian (su3_matrix * m3, anti_hermitmat * ah3);
void random_anti_hermitian (anti_hermitmat * mat_antihermit,
			    double_prn * prn_pt);
void uncompress_anti_hermitian (anti_hermitmat * mat_anti, su3_matrix * mat);
void compress_anti_hermitian (su3_matrix * mat, anti_hermitmat * mat_anti);
void clear_su3mat (su3_matrix * dest);
void su3mat_copy (su3_matrix * a, su3_matrix * b);
void dumpmat (su3_matrix * m);

void su3_projector (su3_vector * a, su3_vector * b, su3_matrix * c);
complex su3_dot (su3_vector * a, su3_vector * b);
double su3_rdot (su3_vector * a, su3_vector * b);
double magsq_su3vec (su3_vector * a);
void su3vec_copy (su3_vector * a, su3_vector * b);
void dumpvec (su3_vector * v);
void clearvec (su3_vector * v);

void mult_su3_mat_vec (su3_matrix * a, su3_vector * b, su3_vector * c);
void mult_su3_mat_vec_sum (su3_matrix * a, su3_vector * b, su3_vector * c);
void mult_su3_mat_vec_sum_4dir (su3_matrix * a, su3_vector * b0,
				su3_vector * b1, su3_vector * b2,
				su3_vector * b3, su3_vector * c);
void mult_su3_mat_vec_nsum (su3_matrix * a, su3_vector * b, su3_vector * c);
void mult_adj_su3_mat_vec (su3_matrix * a, su3_vector * b, su3_vector * c);
void mult_adj_su3_mat_vec_4dir (su3_matrix * a, su3_vector * b,
				su3_vector * c);
void mult_adj_su3_mat_4vec (su3_matrix * mat, su3_vector * src,
			    su3_vector * dest0, su3_vector * dest1,
			    su3_vector * dest2, su3_vector * dest3);
void mult_adj_su3_mat_vec_sum (su3_matrix * a, su3_vector * b,
			       su3_vector * c);
void mult_adj_su3_mat_vec_nsum (su3_matrix * a, su3_vector * b,
				su3_vector * c);

void add_su3_vector (su3_vector * a, su3_vector * b, su3_vector * c);
void sub_su3_vector (su3_vector * a, su3_vector * b, su3_vector * c);
void sub_four_su3_vecs (su3_vector * a, su3_vector * b1, su3_vector * b2,
			su3_vector * b3, su3_vector * b4);

void scalar_mult_su3_vector (su3_vector * src, double scalar,
			     su3_vector * dest);
void scalar_mult_add_su3_vector (su3_vector * src1, su3_vector * src2,
				 double scalar, su3_vector * dest);
void scalar_mult_sum_su3_vector (su3_vector * src1, su3_vector * src2,
				 double scalar);
void scalar_mult_sub_su3_vector (su3_vector * src1, su3_vector * src2,
				 double scalar, su3_vector * dest);
void scalar_mult_wvec (wilson_vector * src, double s, wilson_vector * dest);
void scalar_mult_hwvec (half_wilson_vector * src, double s,
			half_wilson_vector * dest);
void scalar_mult_add_wvec (wilson_vector * src1, wilson_vector * src2,
			   double scalar, wilson_vector * dest);
void scalar_mult_addtm_wvec (wilson_vector * src1, wilson_vector * src2,
			     double scalar, wilson_vector * dest);
void c_scalar_mult_wvec (wilson_vector * src1, complex * phase,
			 wilson_vector * dest);
void c_scalar_mult_add_wvec (wilson_vector * src1, wilson_vector * src2,
			     complex * phase, wilson_vector * dest);
void c_scalar_mult_add_wvec2 (wilson_vector * src1, wilson_vector * src2,
			      complex s, wilson_vector * dest);
void c_scalar_mult_su3vec (su3_vector * src, complex * phase,
			   su3_vector * dest);
void c_scalar_mult_add_su3vec (su3_vector * v1, complex * phase,
			       su3_vector * v2);
void c_scalar_mult_sub_su3vec (su3_vector * v1, complex * phase,
			       su3_vector * v2);

void left_su2_hit_n (su2_matrix * u, int p, int q, su3_matrix * link);
void right_su2_hit_a (su2_matrix * u, int p, int q, su3_matrix * link);
void dumpsu2 (su2_matrix * u);
void mult_su2_mat_vec_elem_n (su2_matrix * u, complex * x0, complex * x1);
void mult_su2_mat_vec_elem_a (su2_matrix * u, complex * x0, complex * x1);

void mult_mat_wilson_vec (su3_matrix * mat, wilson_vector * src,
			  wilson_vector * dest);
void mult_su3_mat_hwvec (su3_matrix * mat, half_wilson_vector * src,
			 half_wilson_vector * dest);
void mult_adj_mat_wilson_vec (su3_matrix * mat, wilson_vector * src,
			      wilson_vector * dest);
void mult_adj_su3_mat_hwvec (su3_matrix * mat, half_wilson_vector * src,
			     half_wilson_vector * dest);

void add_wilson_vector (wilson_vector * src1, wilson_vector * src2,
			wilson_vector * dest);
void sub_wilson_vector (wilson_vector * src1, wilson_vector * src2,
			wilson_vector * dest);
double magsq_wvec (wilson_vector * src);
complex wvec_dot (wilson_vector * src1, wilson_vector * src2);
complex wvec2_dot (wilson_vector * src1, wilson_vector * src2);
double wvec_rdot (wilson_vector * a, wilson_vector * b);

void wp_shrink (wilson_vector * src, half_wilson_vector * dest,
		int dir, int sign);
void wp_shrink_4dir (wilson_vector * a, half_wilson_vector * b1,
		     half_wilson_vector * b2, half_wilson_vector * b3,
		     half_wilson_vector * b4, int sign);
void wp_grow (half_wilson_vector * src, wilson_vector * dest,
	      int dir, int sign);
void wp_grow_add (half_wilson_vector * src, wilson_vector * dest,
		  int dir, int sign);
void grow_add_four_wvecs (wilson_vector * a, half_wilson_vector * b1,
			  half_wilson_vector * b2, half_wilson_vector * b3,
			  half_wilson_vector * b4, int sign, int sum);
void mult_by_gamma (wilson_vector * src, wilson_vector * dest, int dir);
void mult_by_gamma_left (wilson_matrix * src, wilson_matrix * dest, int dir);
void mult_by_gamma_right (wilson_matrix * src, wilson_matrix * dest, int dir);
void mult_swv_by_gamma_l (spin_wilson_vector * src, spin_wilson_vector * dest,
			  int dir);
void mult_swv_by_gamma_r (spin_wilson_vector * src, spin_wilson_vector * dest,
			  int dir);
void su3_projector_w (wilson_vector * a, wilson_vector * b, su3_matrix * c);
void clear_wvec (wilson_vector * dest);
void copy_wvec (wilson_vector * src, wilson_vector * dest);
void dump_wilson_vec (wilson_vector * src);

double gaussian_rand_no (double_prn * prn_pt);
typedef int int32type;
typedef unsigned int u_int32type;
void byterevn (int32type w[], int n);

void
mult_adj_su3_mat_vec (su3_matrix * a, su3_vector * b, su3_vector * c)
{
  int i;
  register double t, ar, ai, br, bi, cr, ci;
  for (i = 0; i < 3; i++)
    {
      ar = a->e[0][i].real;
      ai = a->e[0][i].imag;

      br = b->c[0].real;
      bi = b->c[0].imag;

      cr = ar * br;
      t = ai * bi;
      cr += t;

      ci = ar * bi;
      t = ai * br;
      ci -= t;

      ar = a->e[1][i].real;
      ai = a->e[1][i].imag;

      br = b->c[1].real;
      bi = b->c[1].imag;

      t = ar * br;
      cr += t;
      t = ai * bi;
      cr += t;

      t = ar * bi;
      ci += t;
      t = ai * br;
      ci -= t;

      ar = a->e[2][i].real;
      ai = a->e[2][i].imag;

      br = b->c[2].real;
      bi = b->c[2].imag;

      t = ar * br;
      cr += t;
      t = ai * bi;
      cr += t;

      t = ar * bi;
      ci += t;
      t = ai * br;
      ci -= t;

      c->c[i].real = cr;
      c->c[i].imag = ci;
    }
}

/* { dg-final { scan-assembler-not "mtvsrd" } } */
