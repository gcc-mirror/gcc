/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -mno-upper-regs-sf" } */

/* PR target/80099 was an issue with -mno-upper-regs-sf.  Test for all variable
   extract types with various -mno-upper-regs-* options.  */

double
d_extract_arg_n (vector double v, unsigned long n)
{
  return __builtin_vec_extract (v, n);
}

float
f_extract_arg_n (vector float v, unsigned long n)
{
  return __builtin_vec_extract (v, n);
}

long
sl_extract_arg_n (vector long v, unsigned long n)
{
  return (long) __builtin_vec_extract (v, n);
}

unsigned long
ul_extract_arg_n (vector unsigned long v, unsigned long n)
{
  return (unsigned long) __builtin_vec_extract (v, n);
}

long
si_extract_arg_n (vector int v, unsigned long n)
{
  return (int) __builtin_vec_extract (v, n);
}

unsigned long
ui_extract_arg_n (vector unsigned int v, unsigned long n)
{
  return (unsigned int) __builtin_vec_extract (v, n);
}

long
ss_extract_arg_n (vector short v, unsigned long n)
{
  return (short) __builtin_vec_extract (v, n);
}

unsigned long
us_extract_arg_n (vector unsigned short v, unsigned long n)
{
  return (unsigned short) __builtin_vec_extract (v, n);
}

long
sc_extract_arg_n (vector signed char v, unsigned long n)
{
  return (signed char) __builtin_vec_extract (v, n);
}

unsigned long
uc_extract_arg_n (vector unsigned char v, unsigned long n)
{
  return (unsigned char) __builtin_vec_extract (v, n);
}


double
d_extract_mem_n (vector double *p, unsigned long n)
{
  return __builtin_vec_extract (*p, n);
}

float
f_extract_mem_n (vector float *p, unsigned long n)
{
  return __builtin_vec_extract (*p, n);
}

long
sl_extract_mem_n (vector long *p, unsigned long n)
{
  return (long) __builtin_vec_extract (*p, n);
}

unsigned long
ul_extract_mem_n (vector unsigned long *p, unsigned long n)
{
  return (unsigned long) __builtin_vec_extract (*p, n);
}

long
si_extract_mem_n (vector int *p, unsigned long n)
{
  return (int) __builtin_vec_extract (*p, n);
}

unsigned long
ui_extract_mem_n (vector unsigned int *p, unsigned long n)
{
  return (unsigned int) __builtin_vec_extract (*p, n);
}

long
ss_extract_mem_n (vector short *p, unsigned long n)
{
  return (short) __builtin_vec_extract (*p, n);
}

unsigned long
us_extract_mem_n (vector unsigned short *p, unsigned long n)
{
  return (unsigned short) __builtin_vec_extract (*p, n);
}

long
sc_extract_mem_n (vector signed char *p, unsigned long n)
{
  return (signed char) __builtin_vec_extract (*p, n);
}

unsigned long
uc_extract_mem_n (vector unsigned char *p, unsigned long n)
{
  return (unsigned char) __builtin_vec_extract (*p, n);
}
