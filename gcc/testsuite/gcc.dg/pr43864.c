/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

extern void foo (char*, int);
extern void mysprintf (char *, char *);
extern void myfree (void *);
extern int access (char *, int);
extern int fopen (char *, int);

char *
hprofStartupp (char *outputFileName, char *ctx)
{
  char fileName[1000];
  int fp;
  mysprintf (fileName, outputFileName);
  if (access (fileName, 1) == 0)
    {
      myfree (ctx);
      return 0;
    }

  fp = fopen (fileName, 0);
  if (fp == 0)
    {
      myfree (ctx);
      return 0;
    }

  foo (outputFileName, fp);

  return ctx;
}

/* { dg-final { scan-tree-dump-times "myfree \\(" 1 "pre"} } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
