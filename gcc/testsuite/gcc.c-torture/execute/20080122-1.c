/* PR rtl-optimization/34628 */
/* Origin: Martin Michlmayr <tbm@cyrius.com> */

typedef unsigned short u16;
typedef unsigned char u8;

static void
do_segfault(u8 in_buf[], const u8 out_buf[], const int len)
{
  int i;

  for (i = 0; i < len; i++) {
    asm("");

    in_buf[2*i] = (   out_buf[2*i] | out_buf[(2*i)+1]<<8  ) & 0xFF;

    asm("");

    in_buf[(2*i)+1] =  ( out_buf[2*i] | out_buf[(2*i)+1]<<8 ) >> 8;

    asm("");
  }
}

int main(int argc, char *argv[])
{
  u8 outbuf[32] = "buffer     ";
  u8 inbuf[32] = "\f";

  asm("");
  do_segfault(inbuf, outbuf, 12);
  asm("");

  return 0;
}
