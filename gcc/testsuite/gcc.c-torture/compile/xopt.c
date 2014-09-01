proc1 (a)
     unsigned a;
{
  return (a >> 20) & 0x010fffff;
}

proc2 (a)
     unsigned a;
{
  return (a << 17) & 0xfffff001;
}

proc3 (a)
     unsigned a;
{
  return (a & 0xff00000a) >> 25;
}

proc4 (a)
     unsigned a;
{
  return (a & 0x100000ff) << 25;
}

proc5 (a)
     unsigned a;
{
  return (unsigned char) (a >> 24);
}

proc6 (a)
     unsigned a;
{
  return ((unsigned char) a) << 30;
}
