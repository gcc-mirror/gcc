// PR c++/46670
// { dg-options -std=c++0x }

extern unsigned char __TBB_ReverseByte(unsigned char src);
extern unsigned char *reversed;
template<typename T> T __TBB_ReverseBits(T src)
{
  unsigned char *original = (unsigned char *) &src;
  for( int i = sizeof(T)-1; i--; )
    reversed[i] = __TBB_ReverseByte( original[sizeof(T)-i-1] );
}
