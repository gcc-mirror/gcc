/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

static inline unsigned __int128 rotr(unsigned __int128 input)
{
   return ((input >> 64) | (input << (64)));
}

unsigned __int128 WHIRL_S[256] = {((__int128)0x18186018C07830D8) << 64 |0x18186018C07830D8};
unsigned __int128 whirl(unsigned char x0)
{
   register int t __asm("rdi") = x0&0xFF;
   const unsigned __int128 s4 = WHIRL_S[t];
   register unsigned __int128 tt  __asm("rdi") = rotr(s4);
   asm("":::"memory");
   return tt;
}
