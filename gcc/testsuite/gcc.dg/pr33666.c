/* { dg-do compile } */
/* { dg-options { -std=c99 } } */

/* This used to fail with type-checking enabled because we stripped
   the inner conversion to unsigned int.  */

void __lock_get_list(void *dp)
{
  if (((__SIZE_TYPE__)dp + 1) & ~1ULL)
    ;
}
