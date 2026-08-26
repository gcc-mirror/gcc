/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

/*  verify TLS access for PR27479  */

/* TLS variables of many types.  */
__thread unsigned char t_uc;
__thread signed char t_sc;
__thread unsigned short t_us;
__thread short t_ss;
__thread unsigned int t_ui;
__thread int t_si;
__thread unsigned long t_ul;
__thread long t_sl;
__thread _Bool t_bool;
__thread int *t_ptr;

/* Floating-point.  */
__thread float t_f;
__thread double t_d;

unsigned long
foo_uc (void)
{
  return t_uc;
}

long
foo_sc (void)
{
  return t_sc;
}

unsigned long
foo_us (void)
{
  return t_us;
}

long
foo_ss (void)
{
  return t_ss;
}

unsigned long
foo_ui (void)
{
  return t_ui;
}

long
foo_si (void)
{
  return t_si;
}

unsigned long
foo_ul (void)
{
  return t_ul;
}

long
foo_sl (void)
{
  return t_sl;
}

unsigned long
foo_bool (void)
{
  return t_bool;
}

int *
foo_ptr (void)
{
  return t_ptr;
}

/* Floating-point accessors.  */
float
foo_f (void)
{
  return t_f;
}

double
foo_d (void)
{
  return t_d;
}

/* { dg-final { scan-assembler-not {\maddi\M} } } */
