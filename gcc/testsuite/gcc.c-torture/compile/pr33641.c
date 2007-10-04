/* This failed with type checking enabled.  */

typedef enum { one, two } exp;
extern exp pe;
extern char pt[256];
void psd (void (*f) (void *), void *p);
static void rle (void *e) { }
void
foo (void)
{
  psd ((void (*)(void *)) (rle), (void *) (pt + pe));
}
