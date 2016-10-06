
/* PR tree-optimization/77862 */
/* { dg-do compile } */
/* { dg-options "-O2 -fPIC -w" } */

struct paramed_type
{
};
int a;

int memcmp ( int*, void *, long );

void
cur_token (void)
{
  if ( 1 != a || memcmp ( cur_token, "", a ) )
    return;
  struct paramed_type b = { };
}
