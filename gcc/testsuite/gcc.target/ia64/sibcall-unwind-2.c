/* PR 18987.  This caused an assembler error because we emitted ".restore sp"
   twice.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer -w" } */
/* { dg-final { scan-assembler-times "\\.restore sp" 1 } } */

static void do_date (char *);  
void rfc822_date (char *date)  
{  
  do_date (date);
}
