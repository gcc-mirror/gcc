// PRMS Id: 4689
// Bug: g++ doesn't notice operators overloaded on enumeral types.

enum E { A=5, B=32, C=100 };

E operator|(E a, E b) { return C; };

int main()
{
  return (A|B) != C;
}
