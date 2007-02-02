/* { dg-do compile { target ia64-*-* } } */
/* { dg-options "-O3 -msched-control-spec" } */
typedef long unsigned int size_t;
typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
typedef uint8_t byte;
typedef enum pgpArmor_e
{
  PGPARMOR_ERR_CRC_CHECK = -7, PGPARMOR_ERR_BODY_DECODE =
    -3, PGPARMOR_ERR_UNKNOWN_ARMOR_TYPE = -2, PGPARMOR_ERR_NO_BEGIN_PGP =
    -1, PGPARMOR_NONE = 0, PGPARMOR_MESSAGE = 1, PGPARMOR_PUBKEY =
    5, PGPARMOR_PRIVKEY = 6, PGPARMOR_SECKEY = 7
}
pgpArmor;
pgpCRC (const byte * octets, size_t len)
{
  unsigned int crc = 0xb704ce;
  int i;
  while (len--)
    {
      for (i = 0; i < 8; i++)
	{
	  crc <<= 1;
	  if (crc & 0x1000000)
	    crc ^= 0x1864cfb;
	}
    }
}
pgpReadPkts (const char *fn, const byte ** pkt, size_t * pktlen)
{
  const byte *b = ((void *) 0);
  const char *enc = ((void *) 0);
  byte *dec;
  size_t declen;
  uint32_t crcpkt, crc;
  int pstate = 0;
  pgpArmor ec = PGPARMOR_ERR_NO_BEGIN_PGP;
    {
      switch (pstate)
	{
	case 0:
	  if (b64decode (enc, (void **) &dec, &declen) != 0)
	    {
	      goto exit;
	    }
	  crc = pgpCRC (dec, declen);
	}
    }
exit:if (ec > PGPARMOR_NONE && pkt)
    *pkt = b;
}
