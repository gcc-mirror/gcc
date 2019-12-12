/* PR tree-optimization/89566 */
/* { dg-do compile } */

typedef struct FILE { int i; } FILE;
#ifdef __cplusplus
extern "C"
#endif
int fprintf (FILE *, const char *, ...);

int
main ()
{
  ((void (*)()) fprintf) ();	// { dg-warning "function called through a non-compatible type" "" { target c } }
  return 0;
}
