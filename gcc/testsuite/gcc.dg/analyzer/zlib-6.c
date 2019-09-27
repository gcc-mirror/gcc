typedef unsigned char Byte;
typedef unsigned int uInt;
typedef unsigned long uLong;

typedef struct z_stream_s {
  Byte *next_in;
  uInt avail_in;
  uLong total_in;
} z_stream;

typedef struct inflate_blocks_state {
  uInt bitk;
  uLong bitb;
  Byte *write;
} inflate_blocks_statef;

extern int inflate_flush(inflate_blocks_statef *, z_stream *, int);

int inflate_blocks(inflate_blocks_statef *s, z_stream *z, int r) {
  uInt t;
  uLong b;
  uInt k;
  Byte *p;
  uInt n;
  Byte *q;
  uInt m;

  while (k < (3)) {
    {
      if (n)
        r = 0;
      else {
        {
	  s->bitb = b;
	  s->bitk = k;
	  z->avail_in = n;
	  z->total_in += p - z->next_in;
	  z->next_in = p;
          s->write = q;
        }
        return inflate_flush(s, z, r);
      }
    };
    b |= ((uLong)(n--, *p++)) << k; /* { dg-warning "use of uninitialized value" } */
    k += 8;
  }
}
