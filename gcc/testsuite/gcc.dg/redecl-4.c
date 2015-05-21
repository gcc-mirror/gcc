/* Test for multiple declarations and composite types, with built-in
   functions.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=c89 -Wformat -g" } */

void
f (void)
{
  int printf;
  int strcmp;
  {
    int printf (const char *, ...);
    int strcmp ();
    /* Should get format warnings even though the built-in declaration
       isn't "visible".  */
    printf (
	    "%s", 1); /* { dg-warning "8:format" } */
    /* The type of strcmp here should have no prototype.  */
    if (0)
      strcmp (1);
    /* Likewise, implicitly declared memcmp.  */
    if (0)
      memcmp (1);
  }
}

/* Should still diagnose incompatible prototype for strcmp.  */
int strcmp (void); /* { dg-error "conflict" } */
