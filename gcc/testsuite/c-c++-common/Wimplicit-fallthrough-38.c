/* { dg-do compile } */
/* { dg-options "-Wimplicit-fallthrough=3" } */

#define FOO \
int				\
foo (int a)			\
{				\
  switch (a)			\
    {				\
    case 1:			\
      ++a;			\
      /* FALLTHRU */		\
    case 2:			\
      ++a;			\
      /* FALLTHRU */		\
    ca##se 3:			\
      ++a;			\
    default:			\
      break;			\
    }				\
  return a;			\
}

FOO
