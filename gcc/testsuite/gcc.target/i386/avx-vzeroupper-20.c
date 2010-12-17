/* { dg-do compile } */
/* { dg-options "-O3 -mavx -mtune=generic -dp" } */

extern void free (void *);
void
bar (void *ncstrp)
{
  if(ncstrp==((void *)0))
    return;
  free(ncstrp);
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
