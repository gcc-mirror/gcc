/* PR middle-end/46388 */

struct S;
struct T
{
  struct S *t;
};
extern struct S s, u;

void
foo (void)
{
  ((struct T *) &u)->t = &s;
}
