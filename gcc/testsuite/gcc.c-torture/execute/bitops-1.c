/* PR tree-optimization/110726 */

#define DECLS(n,VOL)			\
__attribute__((noinline,noclone))	\
int h##n(VOL int A, VOL int B){		\
    return (A | B) & (A == B);		\
}					\
__attribute__((noinline,noclone))	\
int i##n(VOL int A, VOL int B){		\
    return A | (A == B);		\
}					\
__attribute__((noinline,noclone))	\
int k##n(VOL int A, VOL int B){		\
    return (A & B) | (A == B);		\
}					\

DECLS(0,)
DECLS(1,volatile)

int values[] = { 0, 1, 2, 3, -1, -2, -3, 0x10080 };
int numvalues = sizeof(values)/sizeof(values[0]);

int main(){
    for(int A = 0; A < numvalues; A++)
      for(int B = 0; B < numvalues; B++)
	{
	  int a = values[A];
	  int b = values[B];
	  if (h0 (a, b) != h1 (a, b)) __builtin_abort();
	  if (i0 (a, b) != i1 (a, b)) __builtin_abort();
	  if (k0 (a, b) != k1 (a, b)) __builtin_abort();
	}
}
