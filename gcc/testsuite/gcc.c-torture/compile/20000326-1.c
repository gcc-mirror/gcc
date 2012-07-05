long sys_reboot(int magic1, int magic2, int cmd, void * arg)
{
  switch (cmd) {
  case 0x89ABCDEF:
    return 1;

  case 0x00000000:
    return 2;

  case 0xCDEF0123:
    return 3;

  case 0x4321FEDC:
    return 4;

  case 0xA1B2C3D4:
    return 5;

  default:
    return 0;
  };
}
