/* { dg-options "-O2 -fdump-tree-optimized" } */
void link_error ();
void g ()
{
  const char **language_names;

  language_names = new const char *[6];

  const char **language_names_p = language_names;

  language_names_p++;
  language_names_p++;
  language_names_p++;

  if ( (language_names_p) - (language_names+3) != 0)
    link_error();
  delete[] language_names;
}
/* We should have removed the link_error on the gimple level as GCC should
   be able to tell that language_names_p is the same as language_names+3.  */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized" } } */
