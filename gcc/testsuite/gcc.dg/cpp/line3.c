/* { dg-do compile } */
/* { dg-options "-Wwrite-strings" } */ 
/* The purpose of this test is to ensure that line numbers in diagnostics
   are accurate after macros whose arguments contain newlines and are
   substituted multiple times.  The semicolons are on separate lines because
   #line can only correct numbering on line boundaries.  */
#define one(x) x
#define two(x) x x
#define four(x) two(x) two(x)

int
main(void)
{
  char *A;

  A = "text";		/* { dg-warning "discards qualifiers" "case zero" } */
  A = one("text"	/* { dg-warning "discards qualifiers" "case one" } */
	  "text")
	;
  A = two("text"	/* { dg-warning "discards qualifiers" "case two" } */
	  "text")
	;
  A = four("text"	/* { dg-warning "discards qualifiers" "case four" } */
	   "text")
	;

  return 0;
}
