/* PR middle-end/55150 */
/* { dg-do compile } */
/* { dg-options "-Os -g" } */
/* { dg-require-effective-target int32plus } */

typedef unsigned int KEY_TABLE_TYPE[(272 / 4)];
  typedef unsigned int u32;
  typedef unsigned char u8;
  static const u32 Camellia_SBOX[][256] = {
  };
   static const u32 SIGMA[] = {
    0xa09e667f, 0x3bcc908b, 0xb67ae858, 0x4caa73b2, 0xc6ef372f, 0xe94f82be,     0x54ff53a5, 0xf1d36f1c, 0x10e527fa, 0xde682d1d, 0xb05688c2, 0xb3e6c1fd };
   int Camellia_Ekeygen (int keyBitLength, const u8 * rawKey, KEY_TABLE_TYPE k) {
    register u32 s0, s1, s2, s3;
    k[0] = s0 = (    	 {
    	 u32 r = *(const u32 *) (rawKey);
  r;
 }
    );
    k[2] = s2 = (		{
    	 u32 r = *(const u32 *) (rawKey + 8);
  r;
 }
    );
    k[3] = s3 = (		{
    	 u32 r = *(const u32 *) (rawKey + 12);
  r;
 }
    );
    if (keyBitLength != 128)     {
        k[8] = s0 = ( 		     {
  	         u32 r = *(const u32 *) (rawKey + 16);
  r;
 }
        );
        if (keyBitLength == 192)	{
  	  k[10] = s2 = ~s0;
  	    k[11] = s3 = ~s1;
  	    }
      }
    s0 ^= k[0], s1 ^= k[1], s2 ^= k[2], s3 ^= k[3];
    if (keyBitLength == 128)     {
        k[4] = s0, k[5] = s1, k[6] = s2, k[7] = s3;
      }
    else     {
        k[12] = s0, k[13] = s1, k[14] = s2, k[15] = s3;
        s0 ^= k[8], s1 ^= k[9], s2 ^= k[10], s3 ^= k[11];
        do    {
  	  register u32 _t0, _t1, _t2, _t3;
  	    _t0 = s2 ^ ((SIGMA + 10))[0];
  	      _t3 ^= Camellia_SBOX[3][(_t0 >> 8) & 0xff];
  	        s1 ^= _t3;
  		}
        while (0);
        do    {
  	  u32 _t0 = s0 >> (32 - 30);
  	    s2 = (s2 << 30) | (s3 >> (32 - 30));
  	      s3 = (s3 << 30) | _t0;
  	      }
        while (0);
        k[40] = s0, k[41] = s1, k[42] = s2, k[43] = s3;
        k[64] = s1, k[65] = s2, k[66] = s3, k[67] = s0;
        s0 = k[8], s1 = k[9], s2 = k[10], s3 = k[11];
        k[36] = s0, k[37] = s1, k[38] = s2, k[39] = s3;
        s0 = k[12], s1 = k[13], s2 = k[14], s3 = k[15];
        do   {
  	  s1 = (s1 << 15) | (s2 >> (32 - 15));
  	  }
        while (0);
        k[12] = s0, k[13] = s1, k[14] = s2, k[15] = s3;
        k[44] = s1, k[45] = s2, k[46] = s3, k[47] = s0;
      }
  }
