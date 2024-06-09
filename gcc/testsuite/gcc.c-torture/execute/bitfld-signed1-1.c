/* PR tree-optimization/114666 */
/* We used to miscompile this to be always aborting
   due to the use of the signed 1bit into the COND_EXPR. */

struct {
  signed a : 1;
} b = {-1};
char c;
int main()
{
  if ((b.a ^ 1UL) < 3)
    __builtin_abort();
}
