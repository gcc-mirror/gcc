/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "bn " } } */
/* { dg-final { scan-assembler "b\[np\] " } } */

unsigned short   a_below __attribute__((__BELOW100__));
unsigned short   b_below __attribute__((__BELOW100__));
unsigned short * a_ptr = & a_below;
unsigned short * b_ptr = & b_below;

char *
foo (void)
{
  if (a_below & 0x0100)
    {
      if (b_below & 0x0100)
	return "Fail";
      return "Success";
    }

  return "Fail";
}

char *
bar (void)
{
  *a_ptr = 0x0100;
  *b_ptr = 0xfeff;
  return foo ();
}
