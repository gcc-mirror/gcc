/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR tree-optimization/115275 */

template<class T>
inline const T &
min(const T &a, const T &b)
{
  return a < b ? a : b;
}

template<class T>
inline const T &
max(const T &a, const T &b)
{
  return a < b ? b : a;
}


unsigned short m, a, b;
void removeme ();
void fn(unsigned short f) {
    if(
    (min(max(f, a) ? f : 0U, 84991U))
    -
    (min(max(f, b) ? f : 0U, 84991U))
    )
    {
      removeme();
    }
}

/* removeme call should be optimized out.  */

/* { dg-final { scan-tree-dump-not "removeme " "optimized" } } */
