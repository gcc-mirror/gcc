/* { dg-do compile } */

void base64_encode(const char *table64,
                   const char *inputbuff, int insize,
                   char * __restrict output)
{
  const unsigned char *in = (const unsigned char *)inputbuff;

  while(insize >= 3) {
    *output++ = table64[ in[0] >> 2 ];
    *output++ = table64[ ((in[0] & 0x03) << 4) | (in[1] >> 4) ];
    *output++ = table64[ ((in[1] & 0x0F) << 2) | ((in[2] & 0xC0) >> 6) ];
    *output++ = table64[ in[2] & 0x3F ];
    insize -= 3;
    in += 3;
  }
}
