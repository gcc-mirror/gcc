typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;

static unsigned long S[1][1]={0x00820200};

static int body(out0,out1,ks,Eswap0,Eswap1)
ulong *out0,*out1;
int *ks;
ulong Eswap0,Eswap1;
{
  register unsigned long l,r,t,u,v;
  register unsigned long *s;
  register int i,j;
  register unsigned long E0,E1;

  l=0;
  r=0;

  s=(ulong *)ks;
  E0=Eswap0;
  E1=Eswap1;

  for (i=0; i<(16 *2); i+=4)
    {
      v=(r^(r>>16));
      u=(v&E0);
      v=(v&E1);
      u=(u^(u<<16))^r^s[  i  ];
      t=(v^(v<<16))^r^s[  i+1];
      t=(t>>4)|(t<<28);
      l^=S[1][(t)&0x3f]| S[3][(t>> 8)&0x3f]| S[5][(t>>16)&0x3f]| S[7][(t>>24)&0x3f]| S[0][(u)&0x3f]| S[2][(u>> 8)&0x3f]| S[4][(u>>16)&0x3f]| S[6][(u>>24)&0x3f];
      v=(l^(l>>16));
      u=(v&E0);
      v=(v&E1);
      u=(u^(u<<16))^l^s[  i+2  ];
      t=(v^(v<<16))^l^s[  i+2+1];
      t=(t>>4)|(t<<28);
      r^=	S[1][(t    )&0x3f];
    }
  t=l;
  l=r;
  r=t;

  t=r;
  r=(l>>1)|(l<<31);
  l=(t>>1)|(t<<31);

  *out0=l;
  *out1=r;
  return(0);
}
