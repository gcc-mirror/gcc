/* PR target/13789 */
/* Failed on SPARC due to a bug in store_expr.  */

void *foo (void *c)
{
  void *a = __builtin_extract_return_addr (c);
  return a;
}
