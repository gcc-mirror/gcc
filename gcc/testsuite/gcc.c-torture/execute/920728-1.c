/* { dg-additional-options "-std=gnu89" } */
typedef struct {int dims[0]; } *A;

f(unsigned long obj)
{
  unsigned char y = obj >> 24;
  y &= ~4;

  if ((y==0)||(y!=251  ))
    abort();

  if(((int)obj&7)!=7)return;

  REST_OF_CODE_JUST_HERE_TO_TRIGGER_THE_BUG:

  {
    unsigned char t = obj >> 24;
    if (!(t==0)&&(t<=0x03))
      return 0;
    return ((A)(obj&0x00FFFFFFL))->dims[1];
  }
}

long g(){return 0xff000000L;}
main (){int x;f(g());exit(0);}
