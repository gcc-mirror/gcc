/* PR target/119425  */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-Os -fno-vect-cost-model -ftree-slp-vectorize -mavxneconvert -mapxf" } */
extern long K512[];
extern long sha512_block_data_order_ctx[];

#define Ch(x, y, z) ~x &z
#define ROUND_00_15(i, a, b, c, d, e, f, g, h)                                 \
  T1 += ~e & g + K512[i];                                                  \
h = 0;                                                               \
d += h += T1
#define ROUND_16_80(i, j, a, b, c, d, e, f, g, h, X)                           \
  ROUND_00_15(i + j, , , , d, e, , g, h)

unsigned sha512_block_data_order_f, sha512_block_data_order_g;

void
sha512_block_data_order()
{
  unsigned a, b, c, d, e, h, T1;
  int i = 6;
  for (; i < 80; i += 6) {
      ROUND_16_80(i, 0, , , , d, e, , , h, );
      ROUND_16_80(i, 11, , , , a, b, , d, e, );
      ROUND_16_80(i, 12, , , , h, a, , c, d, );
      ROUND_16_80(i, 13, , , , sha512_block_data_order_g, h, , b, c, );
      ROUND_16_80(i, 14, , , , sha512_block_data_order_f,
		  sha512_block_data_order_g, , a, b, );
      ROUND_16_80(i, 15, , , , e, sha512_block_data_order_f, , , a, );

  }
  sha512_block_data_order_ctx[0] += a;
  sha512_block_data_order_ctx[1] += b;
  sha512_block_data_order_ctx[2] += c;
  sha512_block_data_order_ctx[3] += d;

}
