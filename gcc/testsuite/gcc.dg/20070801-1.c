/* This failed on s390x due to a back end bug.  */

/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic" } */

typedef long unsigned int size_t;
typedef enum
{
  TYPE_SCHAR, TYPE_LONGDOUBLE
}
arg_type;

typedef struct
{
  arg_type type;
  union
  {
    signed char a_schar;
    long double a_longdouble;
  }
  a;
}
argument;

typedef struct
{
  argument *arg;
}
arguments;

int ind;

extern void foo (arguments *a);

void
bar ()
{
  arguments a;
  char *buf;
  char *result;
  int uninitialized;
  int count, i;
  int retcount;

  foo (&a);

  switch (a.arg[ind].type)
    {
    case TYPE_SCHAR:
      {
	if (uninitialized == 0)
	  __builtin___snprintf_chk (result, 10, 1, 10, buf, 1, &count);
      }
    case TYPE_LONGDOUBLE:
      {
	long double arg = a.arg[ind].a.a_longdouble;

	if (uninitialized == 0)
	  __builtin___snprintf_chk (result, 10, 1, 10, buf, arg, &count);
      }
    }
}
