/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

template <int ROT, typename T>
inline T rotr(T input)
{
   return static_cast<T>((input >> ROT) | (input << (8 * sizeof(T) - ROT)));
}

unsigned long long WHIRL_S[256] = {0x18186018C07830D8};
unsigned long long whirl(unsigned char x0)
{
   const unsigned long long s4 = WHIRL_S[x0&0xFF];
   return rotr<32>(s4);
}
/* { dg-final { scan-assembler-not "movl\tWHIRL_S\\+4\\(,%eax,8\\), %eax" } } */
