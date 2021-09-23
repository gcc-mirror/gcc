/* PR tree-optimization/78888 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void kill (void);
void keep (void);

void g (int x)
{
  if (__builtin_toupper ((unsigned char)x) == 'a')
    kill ();
  if (__builtin_toupper ((unsigned char)x) == 'z')
    kill ();
  if (__builtin_tolower ((unsigned char)x) == 'A')
    kill ();
  if (__builtin_tolower ((unsigned char)x) == 'Z')
    kill ();

  if (__builtin_toupper ((unsigned char)x) == 'A')
    keep ();
  if (__builtin_toupper ((unsigned char)x) == 'Z')
    keep ();
  if (__builtin_tolower ((unsigned char)x) == 'a')
    keep ();
  if (__builtin_tolower ((unsigned char)x) == 'z')
    keep ();
}
/* { dg-final { scan-tree-dump-not "kill" "evrp" } }  */
/* { dg-final { scan-tree-dump-times "keep" 4 "evrp"} } */
