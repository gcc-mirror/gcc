/* Test :: token handling in asm.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23" } */

void
f (void)
{
  asm ("");
  asm ("" : );
  asm ("" : :);
  asm ("" ::);
  asm ("" : : :);
  asm ("" :: :);
  asm ("" : ::);
  asm goto ("" : : : : lab);
  asm goto ("" :: : : lab);
  asm goto ("" : :: : lab);
  asm goto ("" : : :: lab);
  asm goto ("" :: :: lab);
 lab: ;
  /* Test errors when :: is at the end of asm and only one : allowed.  */
  asm ("" : : ::); /* { dg-error "expected" } */
  asm ("" :: ::); /* { dg-error "expected" } */
  asm goto ("" : : : :: lab); /* { dg-error "expected" } */
  asm goto ("" :: : :: lab); /* { dg-error "expected" } */
  asm goto ("" : :: :: lab); /* { dg-error "expected" } */
}
