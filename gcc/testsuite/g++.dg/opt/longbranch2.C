// PR target/11689
// Originator: thor@math.tu-berlin.de

// { dg-do compile }
// { dg-options "-O3 -funroll-loops -mcpu=k6 -fomit-frame-pointer" { target i?86-*-*  } }

// This used to fail to assemble because of an out-of-range 'loop' instructions.


class JKeeper {
public:
  unsigned long a0;
};

class EBCOTLut : public JKeeper {
  unsigned char a1[1<<8];   
  unsigned char a2[1<<8];
  unsigned char a3[1<<8];
  long          a4[1<<9];
public:
  EBCOTLut(void);
};

EBCOTLut::EBCOTLut(void)
{
  unsigned char inter[36];   // intermediate lookup table;
  unsigned long i;
  for(i=0;i<36;i++) {
    inter[i] = 0;
  }
  for(i=1;i<16;i++) {
    a1[i | (1<<7)] = 8<<1;
    a1[i | (1<<6)] = 8<<1;
  }
  for(i=0;i < ((1<<9)-1);i++) {
    int ds = (i>>0) & 0x01;    // significance of DOWN
    int us = (i>>1) & 0x01;    // significance of UP
    int rs = (i>>2) & 0x01;    // significance of RIGHT
    int ls = (i>>3) & 0x01;    // significance of LEFT
    int dn = (i>>5) & 0x01;    // sign of DOWN
    int un = (i>>6) & 0x01;    // sign of UP
    int rn = (i>>7) & 0x01;    // sign of RIGHT
    int ln = (i>>8) & 0x01;    // sign of LEFT
    int h,v;                   // h and v as in the VM description

    h = ls*(1-ln*2) + rs*(1-2*rn);
    v = us*(1-un*2) + ds*(1-2*dn);
    h = (h >= -1)?(h):(-1);
    v = (v >= -1)?(v):(-1);
    h = (h <=  1)?(h):(1);
    v = (v <=  1)?(v):(1);
    a2[i] = inter[((h+1)<<3) | (v+1)];
    a3[i] = inter[((h+1)<<3) | (v+1)] & (unsigned char)(~1);
  }
  for(i=0;i< 1<<9; i++) {
    a4[i]    = 2*(i-(1<<(9-1)))*(i-(1<<(9-1))) - 
      ((i< (1<<(9-1)))?
       (2*(i-(1<<(9-2)))*(i-(1<<(9-2)))):
       (2*(i-(3<<(9-2)))*(i-(3<<(9-2)))));

  }
} 
