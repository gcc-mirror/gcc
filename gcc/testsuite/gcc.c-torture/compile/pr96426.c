/* PR middle-end/96426 */

typedef long long V __attribute__((vector_size(16)));
typedef double W __attribute__((vector_size(16)));

void
foo (V *v)
{
  __builtin_convertvector (*v, W);
}
