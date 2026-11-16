/* PR tree-optimization/128102 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;
typedef unsigned char u8;

static u8 table_a2b_base64[256];

__attribute__ ((noinline, noclone)) size_t
decode_b64 (const u8 *restrict src, size_t len, u8 *restrict dst)
{
  u8 *out = dst;
  int quad_pos = 0;
  u8 leftchar = 0;

  for (size_t i = 0; i < len; i++)
    {
      u8 this_ch = src[i];

      if (this_ch == '=')
	continue;

      this_ch = table_a2b_base64[this_ch];
      if (this_ch >= 64)
	continue;

      switch (quad_pos)
	{
	case 0:
	  quad_pos = 1;
	  leftchar = this_ch;
	  break;
	case 1:
	  quad_pos = 2;
	  *out++ = (u8) ((leftchar << 2) | (this_ch >> 4));
	  leftchar = this_ch & 0x0f;
	  break;
	case 2:
	  quad_pos = 3;
	  *out++ = (u8) ((leftchar << 4) | (this_ch >> 2));
	  leftchar = this_ch & 0x03;
	  break;
	case 3:
	  quad_pos = 0;
	  *out++ = (u8) ((leftchar << 6) | this_ch);
	  leftchar = 0;
	  break;
	}
    }
  return (size_t) (out - dst);
}

int
main (void)
{
  static u8 src[256], dst[256];

  for (int i = 0; i < 256; i++)
    table_a2b_base64[i] = (u8) (i & 63);

  return (int) decode_b64 (src, sizeof src, dst);
}

/* Threading the loop back edge replicates the loop body, and quad_pos
   becomes a constant on every copy, so the '=' test of the source loop
   is left 11 times.  Without the fix the loop stays rolled and the test
   appears only once.  */
/* { dg-final { scan-tree-dump-times "== 61" 11 "optimized" } } */
