/* PR debug/43058 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */
/* { dg-timeout-factor 4 } */

extern void *f1 (void *, void *, void *);
extern void *f2 (const char *, int, int, int, void *(*) ());
extern void *f3 (const char *);
extern void *f4 (void *s);
extern void *f5 (void *);

void test (void)
{
#define X1 f1 (f2 ("a", 1, 0, 0, f5), \
	       f4 (({ const char *a = "b"; f3 (a); })), \
	       ({ const char *a = "c"; f3 (a); }));
#define X2 X1 X1 X1 X1 X1 X1 X1 X1 X1 X1
#define X3 X2 X2 X2 X2 X2 X2 X2 X2 X2 X2
#define X4 X3 X3 X3 X3 X3 X3 X3 X3 X3 X3
  X4 X4
}
