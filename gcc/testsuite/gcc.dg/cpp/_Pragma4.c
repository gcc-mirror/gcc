/* { dg-do preprocess } */

/* Based on Debian GNATS PR 157416.  3 Sep 2002.  */

#define b foo _Pragma ("bar") baz
a b c 

/*
   { dg-final { if ![file exists _Pragma4.i] { return }                   } }
   { dg-final { if { [grep _Pragma4.i "#pragma bar "] != "" } { return }  } }
   { dg-final { fail "_Pragma4.c: #pragma appearing on its own line"      } }
*/
