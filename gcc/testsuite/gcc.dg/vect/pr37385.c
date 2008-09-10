/* Testcase by Martin Michlmayr <tbm@cyrius.com> */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

typedef int int_t;
typedef void (*fun_t) (int);
fun_t fun_tab[400] __attribute__ ((__aligned__(16)));

void foo (int_t a);

void
bar ()
{
  int i;

  for (i = 0; i < 400; i++)
      fun_tab[i] = foo;
}

/* { dg-final { cleanup-tree-dump "vect" } } */
