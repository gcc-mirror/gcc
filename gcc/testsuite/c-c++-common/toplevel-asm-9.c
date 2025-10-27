/* PR middle-end/122133 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

extern int v[42], w;
int x[42], y;
void foo (void);
void bar (void) {}

asm ("# %cc[foo]: %cc[v]: %cc[w]: %cc[bar] %cc[x] %cc[y]"
     :: [foo] ":" (foo), [v] ":" (v), [w] ":" (&w),
	[bar] "-i" (bar), [x] "-s" (x), [y] "-s" (&y));
