/* { dg-do compile } */
/* { dg-prune-output "conflicting types for built-in" } */

void *memmove ();

void *
bar ()
{
  return memmove ();
}

/* { dg-prune-output "\\\[-Wbuiltin-declaration-mismatch]" } */
