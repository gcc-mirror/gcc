/* Verify that cpp doesn't screw up the line numbering when a macro argument
   extends over multiple lines.  */
/* { dg-do compile } */

#define FOO(x)  /* nothing */

void
func(void)
{
  FOO(i
      = 4)
  else;  /* { dg-error "parse error|syntax error|expected" "error on this line" { target *-*-* } { 12 } } */ 
}
