/* { dg-do compile } */
/* { dg-options "-mcmse" } */

int __attribute__ ((cmse_nonsecure_call)) (*ns_foo) (void);
int (*s_bar) (void);
int __attribute__ ((cmse_nonsecure_call)) (**ns_foo2) (void);
int (**s_bar2) (void);

typedef int __attribute__ ((cmse_nonsecure_call)) ns_foo_t (void);
typedef int s_bar_t (void);
typedef int __attribute__ ((cmse_nonsecure_call)) (* ns_foo_ptr) (void);
typedef int (*s_bar_ptr) (void);

int nonsecure0 (ns_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

int nonsecure1 (ns_foo_t ** ns_foo_p)
{
  return (*ns_foo_p) ();
}

int nonsecure2 (ns_foo_ptr ns_foo_p)
{
  return ns_foo_p ();
}
int nonsecure3 (ns_foo_ptr * ns_foo_p)
{
  return (*ns_foo_p) ();
}

int secure0 (s_bar_t * s_bar_p)
{
  return s_bar_p ();
}

int secure1 (s_bar_t ** s_bar_p)
{
  return (*s_bar_p) ();
}

int secure2 (s_bar_ptr s_bar_p)
{
  return s_bar_p ();
}

int secure3 (s_bar_ptr * s_bar_p)
{
  return (*s_bar_p) ();
}

int nonsecure4 (void)
{
  return ns_foo ();
}

int nonsecure5 (void)
{
  return (*ns_foo2) ();
}

int secure4 (void)
{
  return s_bar ();
}

int secure5 (void)
{
  return (*s_bar2) ();
}
/* { dg-final { scan-assembler-times "bl\\s+__gnu_cmse_nonsecure_call" 6 } } */
