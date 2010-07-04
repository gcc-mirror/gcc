// { dg-do compile }
unsigned int mEvictionRank[(1 << 5)];
void Unswap(int i)
{ 
  mEvictionRank[i] = ({ unsigned int __v = i; __v; });
}
