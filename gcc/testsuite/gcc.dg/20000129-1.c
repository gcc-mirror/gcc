/* Regression test for trigraph warnings.  Under certain conditions we
   got the line numbers wrong.
   Originally exposed by XEmacs and reported by Martin Buchholz
   <martin@xemacs.org>; this test case is synthetic.  */

/* { dg-do preprocess } */
/* { dg-options -Wall } */

#define some_macro \
	blah	\
	blah	\
	blah	\
	blah

??>	/* { dg-warning "trigraph ..." "trigraph encountered" } */
