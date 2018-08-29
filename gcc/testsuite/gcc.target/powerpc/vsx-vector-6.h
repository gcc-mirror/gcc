/* This test code is included into vsx-vector-6-be.c and vsx-vector-6-le.c.  
   The two files have the tests for the number of instructions generated for
   LE versus BE.  */

#include <altivec.h>

void foo (vector double *out, vector double *in, vector long *p_l, vector bool long *p_b,
	  vector unsigned char *p_uc, int *i, vector float *p_f,
	  vector bool char *outbc, vector bool int *outbi,
	  vector bool short *outbsi, vector int *outsi,
	  vector unsigned int *outui, vector signed char *outsc,
	  vector unsigned char *outuc)
{
  vector double in0 = in[0];
  vector double in1 = in[1];
  vector double in2 = in[2];
  vector long inl = *p_l;
  vector bool long inb = *p_b;
  vector bool long long inbl0;
  vector bool long long inbl1;
  vector unsigned char uc = *p_uc;
  vector float inf0;
  vector float inf1;
  vector float inf2;
  vector char inc0;
  vector char inc1;
  vector bool char inbc0;
  vector bool char inbc1;
  vector bool short inbs0;
  vector bool short inbs1;
  vector bool int inbi0;
  vector bool int inbi1;
  vector signed short int inssi0, inssi1;
  vector unsigned short int inusi0, inusi1;
  vector signed int insi0, insi1;
  vector unsigned int inui0, inui1;
  vector unsigned char inuc0, inuc1;
  
  *out++ = vec_abs (in0);
  *out++ = vec_add (in0, in1);
  *out++ = vec_and (in0, in1);
  *out++ = vec_and (in0, inb);
  *out++ = vec_and (inb, in0);
  *out++ = vec_andc (in0, in1);
  *out++ = vec_andc (in0, inb);
  *out++ = vec_andc (inb, in0);
  *out++ = vec_andc (inbl0, in0);
  *out++ = vec_andc (in0, inbl0);

  *out++ = vec_ceil (in0);
  *p_b++ = vec_cmpeq (in0, in1);
  *p_b++ = vec_cmpgt (in0, in1);
  *p_b++ = vec_cmpge (in0, in1);
  *p_b++ = vec_cmplt (in0, in1);
  *p_b++ = vec_cmple (in0, in1);
  *out++ = vec_div (in0, in1);
  *out++ = vec_floor (in0);
  *out++ = vec_madd (in0, in1, in2);
  *out++ = vec_msub (in0, in1, in2);
  *out++ = vec_max (in0, in1);
  *out++ = vec_min (in0, in1);
  *out++ = vec_msub (in0, in1, in2);
  *out++ = vec_mul (in0, in1);
  *out++ = vec_nearbyint (in0);
  *out++ = vec_nmadd (in0, in1, in2);
  *out++ = vec_nmsub (in0, in1, in2);
  *out++ = vec_nor (in0, in1);
  *out++ = vec_or (in0, in1);
  *out++ = vec_or (in0, inb);
  *out++ = vec_or (inb, in0);
  *out++ = vec_perm (in0, in1, uc);
  *out++ = vec_rint (in0);
  *out++ = vec_sel (in0, in1, inl);
  *out++ = vec_sel (in0, in1, inb);
  *out++ = vec_sub (in0, in1);
  *out++ = vec_sqrt (in0);
  *out++ = vec_trunc (in0);
  *out++ = vec_xor (in0, in1);
  *out++ = vec_xor (in0, inb);
  *out++ = vec_xor (inb, in0);

  *i++ = vec_all_eq (in0, in1);
  *i++ = vec_all_ge (in0, in1);
  *i++ = vec_all_gt (in0, in1);
  *i++ = vec_all_le (in0, in1);
  *i++ = vec_all_lt (in0, in1);
  *i++ = vec_all_nan (in0);
  *i++ = vec_all_ne (in0, in1);
  *i++ = vec_all_nge (in0, in1);
  *i++ = vec_all_ngt (in0, in1);
  *i++ = vec_all_nle (in0, in1);
  *i++ = vec_all_nlt (in0, in1);
  *i++ = vec_all_numeric (in0);
  *i++ = vec_any_eq (in0, in1);
  *i++ = vec_any_ge (in0, in1);
  *i++ = vec_any_gt (in0, in1);
  *i++ = vec_any_le (in0, in1);
  *i++ = vec_any_lt (in0, in1);
  *i++ = vec_any_nan (in0);
  *i++ = vec_any_ne (in0, in1);
  *i++ = vec_any_nge (in0, in1);
  *i++ = vec_any_ngt (in0, in1);
  *i++ = vec_any_nle (in0, in1);
  *i++ = vec_any_nlt (in0, in1);
  *i++ = vec_any_numeric (in0);

  *p_f++ = vec_msub (inf0, inf1, inf2);
  *p_f++ = vec_nmsub (inf0, inf1, inf2);
  *p_f++ = vec_nmadd (inf0, inf1, inf2);
  *p_f++ = vec_or (inf0, inf1);
  *p_f++ = vec_trunc (inf0);
  
  *out++ = vec_or (inbl0, in0);
  *out++ = vec_or (in0, inbl0);

  *out++ = vec_nor (in0, in1);

  *outbc++ = vec_nor (inbc0, inbc1);
  *outbc++ = vec_andc (inbc0, inbc1);
  *outbc++ = vec_or (inbc0, inbc1);

  *outuc++ = vec_max (inuc0, inuc1);

  *outbi++ = vec_andc (inbi0, inbi1);
  *outbsi++ = vec_andc (inbs0, inbs1);

  *outbsi++ = vec_andc (inbs0, inbs1);

  *outbi++ = vec_nor (inbi0, inbi1);
  *outbi++ = vec_or (inbi0, inbi1);

  *outbsi++ = vec_nor (inbs0, inbs1);
  *outbsi++ = vec_or (inbs0, inbs1);

  *outsi++ = vec_msums(inssi0, inssi1, insi0);
  *outui++ = vec_msums(inusi0, inusi1, inui0);

  *p_f++ = vec_nor (inf0, inf1);

  *p_f++ = vec_andc (inf0, inf1);
  *p_f++ = vec_andc (inbi0, inf0);
  *p_f++ = vec_andc (inf0, inbi0);

  *in++ = vec_andc (inbl0, in1);
  *in++ = vec_andc (in0, inbl1);
}

int main()
{
  vector double *out;
  vector double *in;
  vector long *p_l;
  vector bool long *p_b;
  vector unsigned char *p_uc;
  int *i;
  vector float *p_f;
  vector bool char *outbc;
  vector bool int *outbi;
  vector bool short *outbsi;
  vector int *outsi;
  vector unsigned int *outui;
  vector signed char *outsc;
  vector unsigned char *outuc;

  foo (out, in, p_l, p_b, p_uc, i, p_f, outbc,
       outbi, outbsi, outsi, outui, outsc, outuc);
}
