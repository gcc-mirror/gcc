/* { dg-do run } */

extern void abort (void);

typedef struct {
    int msgLength;
    unsigned char data[1000];
} SMsg;

typedef struct {
    int dummy;
    int d[0];
} SData;

int condition = 3;

int main()
{
  SMsg msg;
  SData *pData = (SData*)(msg.data);
  unsigned int i = 0;
  for (i = 0; i < 1; i++)
    {
      pData->d[i] = 0;
      if(condition & 1)
	pData->d[i] |= 0x55;
      if(condition & 2)
	pData->d[i] |= 0xaa;
    }
  if (pData->d[0] != 0xff)
    abort ();
  return 0;
}
