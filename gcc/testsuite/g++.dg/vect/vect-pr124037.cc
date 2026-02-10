/* PR tree-optimization/124037 */
/* { dg-require-effective-target mmap } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-additional-options "-std=c++11" } */

struct Token
{
  unsigned t, t1, t2;
  void *a, *b;
  unsigned short Kind;
  unsigned short flags;
  constexpr Token() : Token(0){}
  constexpr Token(int a): t(0), t1(0), t2 (0), a(nullptr),b (nullptr),
			  Kind(a), flags(0) {}
  bool isNot(int K) const { return Kind != K; }
};

[[gnu::noipa]]
unsigned getArgLength(const Token *ArgPtr) {
  unsigned NumArgTokens = 0;
  for (; ArgPtr->isNot(0); ++ArgPtr)
    ++NumArgTokens;
  return NumArgTokens;
}

Token tokens[] = {{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},
		  {1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},
		  {1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},
		  {1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},
		  {1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},{1},
		  {1},{1},{1},{1},{1},{1},{}};

int main()
{
  __builtin_printf("%d\n", getArgLength(tokens));
}
/* { dg-final { scan-tree-dump "missed:   not vectorized: relevant stmt not supported: _\[0-9\]+ = ArgPtr_\[0-9\]+->Kind;" "vect" } } */
/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
