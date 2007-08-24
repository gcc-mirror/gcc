static void ConvertAddr (char *saddr, void **addr)
{
  *addr = (void *) &saddr;
}
void DefineSelf (char *addr)
{
  ConvertAddr (addr, (void **) &addr);
  if (addr[0] == 127 && addr[3] == 1)
    ;
}
