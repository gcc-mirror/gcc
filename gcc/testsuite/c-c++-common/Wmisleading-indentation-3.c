/* Verify -Wmisleading-indentation with source-printing.
   This is a subset of Wmisleading-indentation.c.  */

/* { dg-options "-Wmisleading-indentation -fdiagnostics-show-caret" } */
/* { dg-do compile } */

extern int foo (int);
extern int bar (int, int);
extern int flagA;
extern int flagB;
extern int flagC;
extern int flagD;

void
fn_5 (double *a, double *b, double *sum, double *prod)
{
  int i = 0;
  for (i = 0; i < 10; i++) /* { dg-warning "3: this 'for' clause does not guard..." } */
    sum[i] = a[i] * b[i];
    prod[i] = a[i] * b[i]; /* { dg-message "5: ...this statement, but the latter is misleadingly indented as if it is guarded by the 'for'" } */
/* { dg-begin-multiline-output "" }
   for (i = 0; i < 10; i++)
   ^~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
     prod[i] = a[i] * b[i];
     ^~~~
   { dg-end-multiline-output "" } */
}

/* Based on CVE-2014-1266 aka "goto fail" */
int fn_6 (int a, int b, int c)
{
	int err;

	/* ... */
	if ((err = foo (a)) != 0)
		goto fail;
	if ((err = foo (b)) != 0) /* { dg-message "2: this 'if' clause does not guard..." } */
		goto fail;
		goto fail; /* { dg-message "3: ...this statement, but the latter is misleadingly indented as if it is guarded by the 'if'" } */
	if ((err = foo (c)) != 0)
		goto fail;
	/* ... */

/* { dg-begin-multiline-output "" }
  if ((err = foo (b)) != 0)
  ^~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   goto fail;
   ^~~~
   { dg-end-multiline-output "" } */

fail:
	return err;
}

#define FOR_EACH(VAR, START, STOP) \
  for ((VAR) = (START); (VAR) < (STOP); (VAR++)) /* { dg-warning "3: this 'for' clause does not guard..." } */

void fn_14 (void)
{
  int i;
  FOR_EACH (i, 0, 10) /* { dg-message "in expansion of macro .FOR_EACH." } */
    foo (i);
    bar (i, i); /* { dg-message "5: ...this statement, but the latter is misleadingly indented as if it is guarded by the 'for'" } */

/* { dg-begin-multiline-output "" }
   for ((VAR) = (START); (VAR) < (STOP); (VAR++))
   ^
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
   FOR_EACH (i, 0, 10)
   ^~~~~~~~
   { dg-end-multiline-output "" } */
/* { dg-begin-multiline-output "" }
     bar (i, i);
     ^~~
   { dg-end-multiline-output "" } */
}
#undef FOR_EACH
