struct eth_test_pkt {
  unsigned short len;
  unsigned short ctr;
  unsigned char packet[];
} __attribute__ ((packed));
struct eth_test_pkt pkt_unaligned = { .packet = { 0xFC } };
int cmd_unaligned(const void *p)
{
  return memcmp(p, pkt_unaligned.packet, 1);
}

