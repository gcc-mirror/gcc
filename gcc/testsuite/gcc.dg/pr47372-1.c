/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-O2 -fPIC -g" } */

typedef struct FILE FILE;
int _fwalk(int (*)(FILE *));
int __sflush(FILE *);
int
fflush(FILE *fp)
{
  return (_fwalk(__sflush));
}
