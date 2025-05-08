/* { dg-do compile } */
/* { dg-options "-march=sapphirerapids -Ofast" } */
/* { dg-final { scan-assembler-times "vpbroadcastw" 1 } } */

extern short write_picture_p_Vid_0;
extern unsigned short *write_picture_p_2_0_0;
extern int write_picture_p_0, write_picture_p_1, write_picture_i;
void write_picture() {
  unsigned short cr_val = 1 << write_picture_p_Vid_0;
  for (; write_picture_p_1;)
    for (; write_picture_i < write_picture_p_0; write_picture_i++)
      write_picture_p_2_0_0[write_picture_i] = cr_val;
}
