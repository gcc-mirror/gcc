/* Verify that we get errors for trying to put TLS data in 
   sections which can't work.  */

#define A(X)	__attribute__((section(X)))

__thread int i A("foo");		/* Ok */

__thread int j A(".data");  /* { dg-error "causes a section type conflict" "conflict with .data section" { xfail *-*-* } } */

int k A("bar");
__thread int l A("bar");  /* { dg-error "causes a section type conflict" "conflict with user-defined section" } */
