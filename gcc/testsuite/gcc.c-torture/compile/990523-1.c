extern float  decwin[512+32];

int synth_1to1(float  *bandPtr,int channel,unsigned char *out,int *pnt)
{
  static const int step = 2;
  short *samples = (short *) (out+*pnt);

  float  *b0;
  int clip = 0; 
  int bo1;

  {
    register int j;
    float  *window = decwin + 16 - bo1;
    for (j=15;j;j--,b0-=0x20,window-=0x10,samples+=step)
    {
      float  sum;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;
      sum -= *(--window) * *b0++;

      if( ( sum ) > 32767.0) *( samples ) = 0x7fff; ( clip )++;  ;
    }
  }
}
