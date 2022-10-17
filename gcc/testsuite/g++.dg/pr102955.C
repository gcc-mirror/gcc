/* PR debug/102955 */
/* { dg-do compile } */
/* { dg-options "-g -gtoggle" } */

#pragma GCC optimize "0"
struct j
{
  explicit j ();
  ~j ();
};
void g (void)
{
  new j();
}
