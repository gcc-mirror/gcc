/* { dg-options "-fcondition-coverage -ftest-coverage" } */
/* { dg-do run { target native } } */

#include <setjmp.h>
jmp_buf buf;

void noop () {}
int identity (int x) { return x; }

/* This function is a test to verify that the expression isolation does not
   break on a CFG with the right set of complex edges.  The (_ && setjmp)
   created complex edges after the function calls and a circular pair of
   complex edges around the setjmp call.  This triggered a bug when the search
   for right operands only would consider nodes dominated by the left-most
   term, as this would only be the case if the complex edges were removed.  (_
   && setjmp) is undefined behavior, but it does happen in the wild.

   __builtin_setjmp did not trigger this, so we need setjmp from libc.  */
void
setjmp001 (int a, int b, int c)
{
    if (a)  /* conditions(1/2) true(0) */
	    /* conditions(end) */
	noop ();

    if (b)  /* conditions(1/2) false(0) */
	    /* conditions(end) */
	noop ();

    if (c && setjmp (buf))  /* conditions(1/4) true(0 1) false(1) */
			    /* conditions(end) */
	noop ();
}

/* Adapted from freetype-2.13.0 gxvalid/gxvmod.c classic_kern_validate */
int
setjmp002 (int a)
{
    int error = identity(a);

    if (error)	/* conditions(1/2) true(0) */
		/* conditions(end) */
	goto Exit;

   if (a+1) /* conditions(1/2) false(0) */
	    /* conditions(end) */
   {
       noop ();
       if (setjmp (buf))    /* conditions(1/2) true(0) */
			    /* conditions(end) */
	   noop ();

	if (error)  /* conditions(1/2) true(0) */
		    /* conditions(end) */
	    noop ();
   }

   error--;

Exit:
   return error;
}

int
setjmp003 (int a)
{
    /* || setjmp is undefined behavior, so the result here does not have to
       make sense.  It would be nice if the result is not something like 35/4
       conditions covered.  */
    if (a || setjmp (buf)) /* conditions(suppress) */
			   /* conditions(end) */
	a += 12;

    return a;
}

jmp_buf dest;

int
setdest ()
{
    if (setjmp (dest)) /* conditions(2/2) */
	return 1;
    return 2;
}

void
jump ()
{
    /* Protect the longjmp so it will only be done once.  The whole purpose of
       this function is to help test conditions and instrumentation around
       setjmp and its complex edges, as both branches should count towards
       coverage, even when one is taken through longjmp.  If the jump is not
       guarded it can cause an infinite loop as setdest returns to a point in
       main before jump (), leading to an infinite loop.  See PR
       gcov-profile/114720.  */
    static int called_once = 0;
    if (!called_once) /* conditions(suppress) */
    {
	called_once = 1;
	longjmp (dest, 1);
    }
}

int
main ()
{
    setjmp001 (0, 1, 0);
    setjmp002 (0);
    setjmp003 (0);
    setdest ();
    jump ();
}

/* { dg-final { run-gcov conditions { --conditions gcov-22.c } } } */
