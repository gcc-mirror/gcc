/* PR 17322 */

struct s { int a; int b[1]; };
struct s x;
int *y = ((struct s *)&x.a)->b;
