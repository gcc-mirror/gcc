/* Valid initializations.  */
/* { dg-require-effective-target tls_native } */

__thread int i = 42;

static int j;
__thread int *p = &j;

/* Note that this is valid in C++ (unlike C) as a run-time initialization.  */
int *q = &i;

/* Valid because "const int k" is an integral constant expression in C++.  */
__thread const int k = 42;
__thread const int l = k;
