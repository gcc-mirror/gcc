int global_one;

void clobber_register()
{
  *(volatile unsigned char *)(0xE0000000 * global_one) = 0x00;
}
