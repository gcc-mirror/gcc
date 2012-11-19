/* PR tree-optimization/19105 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1-details" } */

enum e
{
  a, b, c, d, e, f, g, h
};

int range1 (enum e v, int x)
{
  return x && v != c && v != d && v != e;
}

int range2 (enum e v, int x)
{
  return x && (v != c && v != d && v != e);
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests v_\[0-9\]*.D. -.2, 2. and -.3, 4.\[\n\r\]* into|Optimizing range tests v_\[0-9\]*.D. -.2, 2. and -.3, 3. and -.4, 4.\[\n\r\]* into" 1 "reassoc1" } } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */

