/* { dg-additional-options "-fPIC" { target fpic } }  */

typedef unsigned long word32;
typedef unsigned char byte;
typedef struct cast256_instance { word32 l_key[96];} cast256_key;
word32 cast256_sbox[4][256] = {};
void
key (cast256_key *key, const word32 *in_key, const int key_len)
{
  word32 i, j, t, u, cm, cr, lk[8], tm[8], tr[8];
  for (i = 0; i < 96; i += 8)
    {
      for (j = 0; j < 8; ++j)
	{
	  tm[j] = cm;
	  cm += 0x6ed9eba1;
	  tr[j] = cr;
	  cr += 17;
	}
      t = (((tm[0] + lk[7]) << ((word32)(tr[0] & 31)))
	   | ((tm[0] + lk[7]) >> (32 - (word32)(tr[0] & 31))));
      u = cast256_sbox[0][((byte)((t) >> (8 * 3)))];
      u ^= cast256_sbox[1][((byte)((t) >> (8 * 2)))];
      lk[7] ^= u;
      for (j = 0; j < 8; ++j)
	{
	  cm += 0x6ed9eba1;
	  tr[j] = cr;
	  cr += 17;
	}
      t = (((tm[0] + lk[7]) << ((word32)(tr[0] & 31)))
	   | ((tm[0] + lk[7]) >> (32 - (word32)(tr[0] & 31))));
      u = cast256_sbox[0][((byte)((t) >> (8 * 3)))];
      u ^= cast256_sbox[1][((byte)((t) >> (8 * 2)))];
      u -= cast256_sbox[2][((byte)((t) >> (8 * 1)))];
      lk[2] ^= u;
      lk[7] ^= u;
      key->l_key[i + 1] = lk[2];
    }
}
