/* m-un-1.c: "un" for "uninitialized" */

/*
From: Jim Wilson <wilson@cygnus.com>
Date: Wed, 6 Jul 1994 13:11:47 -0700
To: dje@cygnus.com
Subject: Re: devo/gcc ChangeLog.fsf stmt.c
Cc: cvs-gcc@cygnus.com, tege@cygnus.com

	How about a test case?  :-)

Compile with -O -Wall and the broken compiler gives you:
tmp.c:6: warning: `k' might be used uninitialized in this function
The fixed compiler (and gcc 2.5.8) gives no warning.

This happens to fix a performance regression in the code generated for
while loops, but that is presumably much much harder to test for.
*/

/* { dg-do compile } */
/* { dg-options "-O -Wall" } */

int
sub ()
{
  int i = 0;
  int j = 0;
  int k;	/* { dg-bogus "`k' might be used uninitialized" "uninitialized warning regression" } */

  while (i == 0 && j == 0)
    {
      k = 10;
      i = sub ();
    }

  return k;
}
