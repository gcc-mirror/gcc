long sys_reboot(int magic1, int magic2, int cmd, void * arg)
{
  switch (cmd) {
  case 0x89ABCDEF:
    break;

  case 0x00000000:
    break;

  case 0xCDEF0123:
    break;

  case 0x4321FEDC:
    break;

  case 0xA1B2C3D4:
    break;

  default:
    break;
  };
  return 0;
}
