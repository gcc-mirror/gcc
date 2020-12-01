/* PR sanitizer/63316 */
/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-skip-if "" { *-*-* }  { "*" } { "-O2" } } */

#ifdef __cplusplus
extern "C" {
#endif
extern void *malloc (__SIZE_TYPE__);
extern void free (void *);
#ifdef __cplusplus
}
#endif

int
main ()
{
  int *p = (int *) malloc (sizeof (int));
  *p = 3;
  asm volatile ("" : : "r" (p) : "memory");
  free (p);
  return 0;
}

