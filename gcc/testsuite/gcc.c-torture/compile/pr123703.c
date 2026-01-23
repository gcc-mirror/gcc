/* PR middle-end/123703 */

struct S { int a; };
struct S abs (int);

struct S
bar (int j)
{
  return abs (j);
}
