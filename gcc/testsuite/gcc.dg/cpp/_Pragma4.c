/* { dg-do preprocess } */

/* Based on Debian GNATS PR 157416.  3 Sep 2002.  */

#define b foo _Pragma ("bar") baz
a b c 

/* { dg-final { scan-file "_Pragma4.i" "(^|\\n)#pragma bar($|\\n)" } } */
