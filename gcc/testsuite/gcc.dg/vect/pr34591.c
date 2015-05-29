/* { dg-do compile } */

int av_resample(int filter_length, short *src, short *filter)
{
    int i;
    int val=0;
    for(i=0; i<filter_length; i++)
      val += src[ i ] * filter[i];
    return val;
}

