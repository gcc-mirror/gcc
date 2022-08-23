/* { dg-do compile } */

__attribute__((target ("arch=armv8-a-typo"))) void
foo ()
{
  /* { dg-message "valid arguments are: \[^\n\r]*(; did you mean 'armv*'?)?"  "" { target *-*-* } .-1 } */
  /* { dg-error "invalid name 'armv8-a-typo' in 'target\\(\"arch=\"\\)' pragma or attribute"  "" { target *-*-* } .-2 } */
  /* { dg-error "pragma or attribute 'target\\(\"arch=armv8-a-typo\"\\)' is not valid"  "" { target *-*-* } .-3 } */
}
