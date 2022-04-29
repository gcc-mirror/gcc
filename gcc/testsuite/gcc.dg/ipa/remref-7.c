/* { dg-do compile } */
/* { dg-options "-O2 -fno-early-inlining -fno-ipa-sra -fdump-ipa-inline --param max-inline-insns-auto=100" }  */

int rglobal = 0;
int g;

int c;
double *array;

/* unused parameter */
static void bar(int *p)
{
  int i;
  for (i = 0; i < c; i++)
    {
      /* something big so that it is inlined second. */
      array[i] = __builtin_exp(array[i]+1)*2;
    }
}

void foo(int *p) {
  g = *p;
  bar(p);
}

void entry()
{
  foo(&rglobal);
}

/* { dg-final { scan-ipa-dump "Removed a reference"  "inline" } }  */
/* { dg-final { scan-ipa-dump "adding LOAD reference"  "inline"  } } */

