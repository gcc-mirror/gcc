/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "testb" } } */
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
