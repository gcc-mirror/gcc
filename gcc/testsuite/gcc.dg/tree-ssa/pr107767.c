/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-switchconv" } */

int firewall2(const unsigned char *restrict data)
{
  const unsigned short dst_port = *((const unsigned short *)data + 32);

  if (dst_port == 15) return 1;
  if (dst_port == 23) return 1;
  if (dst_port == 47) return 1;
  if (dst_port == 45) return 1;
  if (dst_port == 42) return 1;
  if (dst_port == 1) return 1;
  if (dst_port == 2) return 1;
  if (dst_port == 3) return 1;

  return 0;
}

/* { dg-final { scan-tree-dump-not "CSWTCH" "switchconv" } } */
