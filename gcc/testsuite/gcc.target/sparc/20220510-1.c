/* PR target/105292 */
/* Reported by Koakuma <koachan+gccbugs@protonmail.com> */

/* { dg-do compile } */
/* { dg-options "-O3 -mvis2" } */

extern void get_vbytes_v2 (unsigned);

typedef struct {
  unsigned ctt_info;
  unsigned ctt_size;
} ctf_type_t;

typedef struct {
  unsigned short cts_offset;
  unsigned short cts_bits;
} ctf_slice_t;

void flip_types_len (ctf_type_t *t, int bsx1, int bsx2)
{
  const int kind = t->ctt_info;

  get_vbytes_v2 (t->ctt_size);

  if (kind == 4)
    {
      ctf_slice_t *s = (ctf_slice_t *)t;
      s->cts_offset = __builtin_bswap16(bsx1);
      s->cts_bits   = __builtin_bswap16(bsx2);
    }
}
