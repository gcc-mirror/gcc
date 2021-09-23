/* PR target/100305 */

typedef double v2df __attribute__((vector_size(16)));

#define N 4096
void consume (void *);
v2df
foo (void)
{
  double x[N+2];
  consume (x);
  return (v2df) { x[N], x[N + 1] };
}
