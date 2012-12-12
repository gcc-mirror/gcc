/* PR middle-end/55150 */
/* { dg-do compile } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-Os -g -fPIC" } */

typedef unsigned char DES_cblock[8];
 typedef struct DES_ks {
 }
 DES_key_schedule;
 void DES_ede3_cbcm_encrypt (const unsigned char *in, unsigned char *out, 		       long length, DES_key_schedule * ks1, 		       DES_key_schedule * ks2, DES_key_schedule * ks3, 		       DES_cblock * ivec1, DES_cblock * ivec2, int enc) {
   register unsigned long tout0, tout1, xor0, xor1, m0, m1;
   register long l = length;
   unsigned long tin[2];
   unsigned char *iv1, *iv2;
   iv1 = &(*ivec1)[0];
   iv2 = &(*ivec2)[0];
   if (enc)     {
       (m0 = ((unsigned long) (*((iv1)++))), m0 |=        ((unsigned long) (*((iv1)++))) << 8L, m0 |=        ((unsigned long) (*((iv1)++))) << 16L, m0 |=        ((unsigned long) (*((iv1)++))) << 24L);
       (m1 = ((unsigned long) (*((iv1)++))), m1 |=        ((unsigned long) (*((iv1)++))) << 8L, m1 |=        ((unsigned long) (*((iv1)++))) << 16L, m1 |=        ((unsigned long) (*((iv1)++))) << 24L);
       (tout0 = ((unsigned long) (*((iv2)++))), tout0 |=        ((unsigned long) (*((iv2)++))) << 8L, tout0 |=        ((unsigned long) (*((iv2)++))) << 16L, tout0 |=        ((unsigned long) (*((iv2)++))) << 24L);
       for (l -= 8;
 l >= -7;
 l -= 8) 	{
 	  DES_encrypt1 (tin, ks3, 1);
 	  DES_encrypt1 (tin, ks1, 1);
 	}
       (*((iv1)++) = (unsigned char) (((m0)) & 0xff), *((iv1)++) =        (unsigned char) (((m0) >> 8L) & 0xff), *((iv1)++) =        (unsigned char) (((m0) >> 16L) & 0xff), *((iv1)++) =        (unsigned char) (((m0) >> 24L) & 0xff));
       (*((iv1)++) = (unsigned char) (((m1)) & 0xff), *((iv1)++) =        (unsigned char) (((m1) >> 8L) & 0xff), *((iv1)++) =        (unsigned char) (((m1) >> 16L) & 0xff), *((iv1)++) =        (unsigned char) (((m1) >> 24L) & 0xff));
       (*((iv2)++) = (unsigned char) (((tout0)) & 0xff), *((iv2)++) =        (unsigned char) (((tout0) >> 8L) & 0xff), *((iv2)++) =        (unsigned char) (((tout0) >> 16L) & 0xff), *((iv2)++) =        (unsigned char) (((tout0) >> 24L) & 0xff));
     }
   else     {
       (m0 = ((unsigned long) (*((iv1)++))), m0 |=        ((unsigned long) (*((iv1)++))) << 8L, m0 |=        ((unsigned long) (*((iv1)++))) << 16L, m0 |=        ((unsigned long) (*((iv1)++))) << 24L);
       (xor1 = ((unsigned long) (*((iv2)++))), xor1 |=        ((unsigned long) (*((iv2)++))) << 8L, xor1 |=        ((unsigned long) (*((iv2)++))) << 16L, xor1 |=        ((unsigned long) (*((iv2)++))) << 24L);
       for (l -= 8;
 l >= -7;
 l -= 8) 	{
 	  DES_encrypt1 (tin, ks3, 1);
 	  if (l < 0) 	    {
 	      {
 		switch (l + 8) 		  {
 		  case 7: 		    *(--(out)) = (unsigned char) (((tout1) >> 16L) & 0xff);
 		  case 6: 		    *(--(out)) = (unsigned char) (((tout1) >> 8L) & 0xff);
 		  case 5: 		    *(--(out)) = (unsigned char) (((tout1)) & 0xff);
 		  case 4: 		    *(--(out)) = (unsigned char) (((tout0) >> 24L) & 0xff);
 		  case 1: 		    *(--(out)) = (unsigned char) (((tout0)) & 0xff);
 		  }
 	      };
 	    }
 	}
       (*((iv1)++) = (unsigned char) (((m0)) & 0xff), *((iv1)++) =        (unsigned char) (((m0) >> 8L) & 0xff), *((iv1)++) =        (unsigned char) (((m0) >> 16L) & 0xff), *((iv1)++) =        (unsigned char) (((m0) >> 24L) & 0xff));
       (*((iv2)++) = (unsigned char) (((xor0)) & 0xff), *((iv2)++) =        (unsigned char) (((xor0) >> 8L) & 0xff), *((iv2)++) =        (unsigned char) (((xor0) >> 16L) & 0xff), *((iv2)++) =        (unsigned char) (((xor0) >> 24L) & 0xff));
       (*((iv2)++) = (unsigned char) (((xor1)) & 0xff), *((iv2)++) =        (unsigned char) (((xor1) >> 8L) & 0xff), *((iv2)++) =        (unsigned char) (((xor1) >> 16L) & 0xff), *((iv2)++) =        (unsigned char) (((xor1) >> 24L) & 0xff));
     }
 }
