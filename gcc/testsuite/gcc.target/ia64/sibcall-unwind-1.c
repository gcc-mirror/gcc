/* PR 13158.  Emit ".restore sp" for a sibcall.  */
/* { dg-do compile } */
/* { dg-options "-O2 -w" } */
/* { dg-final { scan-assembler-times "\\.restore sp" 1 } } */

static void do_date (char *);  
void rfc822_date (char *date)  
{  
  do_date (date);
}
