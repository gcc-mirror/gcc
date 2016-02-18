/* PR69537, spurious warning because of a missed optimization. */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-short-enums -Wuninitialized" } */

enum clnt_stat {
 RPC_SUCCESS=0,
 RPC_CANTENCODEARGS=1,
};
 
int do_ypcall_tr ();
 
static int
yp_master (char **outname)
{
  // Replacing enum clnt_stat with int avoids the warning.
  enum clnt_stat result;
  result = do_ypcall_tr ();
  if (result != 0)
    return result;
  *outname = __builtin_strdup ("foo");
  return 0;
}
 
int
yp_update (void)
{
  char *master;
  int r;
  if ((r = yp_master (&master)) != 0)
    return r;
  __builtin_free (master); /* { dg-bogus "uninitialized" } */
  return 0;
}
