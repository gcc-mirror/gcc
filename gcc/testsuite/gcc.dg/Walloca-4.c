/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-Walloca-larger-than=5000 -O2" } */

 char *
 _i18n_number_rewrite (char *w, char *rear_ptr)
{

  char *src;
  _Bool use_alloca = (((rear_ptr - w) * sizeof (char)) < 4096U);
  if (use_alloca)
    src = (char *) __builtin_alloca ((rear_ptr - w) * sizeof (char));
  else
    src = (char *) __builtin_malloc ((rear_ptr - w) * sizeof (char));
  return src;
}

/* { dg-prune-output "-Wreturn-local-addr" } */
