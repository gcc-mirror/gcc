/* PR rtl-optimization/18861 */
/* { dg-do compile } */
/* { dg-options "-O2 -floop-optimize2" } */

extern void abort (void);

int
foo (int code)
{
  if (code >= 3)
    switch (code)
      {
      case 3: return 4;
      case 4: return 3;
      case 5: return 6;
      case 6: return 7;
      case 7: return 8;
      case 8: return 5;
      default: abort ();
      }
  switch (code)
    {
    case 3: return 4;
    case 4: return 3;
    case 5: return 6;
    case 6: return 7;
    case 7: return 8;
    case 8: return 5;
    default: abort ();
    }
}
