/* { dg-options "-O2 -fgraphite-identity -ffast-math" } */

void
VBR_encode_frame (int mode_gr, int channels_out, int max_bits[2][2])
{
 int max_nbits_ch[2][2];
 int gr, ch;
 for (gr = 0; gr < mode_gr; ++gr)
   {
     float f[2], s = 0;
     for (ch = 0; ch < channels_out; ++ch)
       if (max_nbits_ch[gr][ch] > 0)
         s += f[ch];
     for (ch = 0; ch < channels_out; ++ch)
       if (s > 0)
         max_nbits_ch[gr][ch] = 7680 * f[ch] / s;
   }
}
