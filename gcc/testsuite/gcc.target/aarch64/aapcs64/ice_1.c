/* Test AAPCS layout

   Empty, i.e. zero-sized, small struct passing used to cause Internal Compiler
   Error.  */

/* { dg-do compile { target aarch64*-*-* } } */

struct AAAA
{

} aaaa;


void named (int, struct AAAA);
void unnamed (int, ...);

void foo ()
{
  name (0, aaaa);
  unnamed (0, aaaa);
}
