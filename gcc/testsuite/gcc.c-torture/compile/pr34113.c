struct sockaddr_in { int sin_addr; };
static void ConvertAddr (struct sockaddr_in *saddr, void **addr)
{
  *addr = (void *) &saddr->sin_addr;
}
unsigned char EnableLocalHost (struct sockaddr_in *ifa_addr)
{
  unsigned char * addr;
  ConvertAddr(ifa_addr, (void **)&addr);
  return addr[0];
}
