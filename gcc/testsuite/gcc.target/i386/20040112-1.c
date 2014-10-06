/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "testb" } } */
void
ftn (char *sp)
{
  char status;

  while (1)
    {
      *sp = 0xE8;
      status = *(volatile char *) sp;
      if (status & 0x80)
	break;
    }
}
