/* PR tree-optimization/21643 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1-details --param logical-op-non-short-circuit=1" } */

int
f1 (unsigned char c)
{
  if (c == 0x22 || c == 0x20 || c < 0x20)
    return 1;
  return 0;
}

int
f2 (unsigned char c)
{
  if (c == 0x22 || c <= 0x20)
    return 1;
  return 0;
}

int
f3 (unsigned char c)
{
  if (c == 0x22)
    return 1;
  if (c == 0x20)
    return 1;
  if (c < 0x20)
    return 1;
  return 0;
}

int
f4 (unsigned char c)
{
  if (c == 0x22 || c == 0x20 || c < 0x20)
    return 2;
  return 0;
}

int
f5 (unsigned char c)
{
  if (c == 0x22 || c <= 0x20)
    return 2;
  return 0;
}

int
f6 (unsigned char c)
{
  if (c == 0x22)
    return 2;
  if (c == 0x20)
    return 2;
  if (c < 0x20)
    return 2;
  return 0;
}

int
f7 (unsigned char c)
{
  if (c != 0x22 && c != 0x20 && c >= 0x20)
    return 0;
  return 1;
}

int
f8 (unsigned char c)
{
  if (c == 0x22 && c <= 0x20)
    return 0;
  return 1;
}

int
f9 (unsigned char c)
{
  if (c == 0x22)
    return 0;
  if (c == 0x20)
    return 0;
  if (c < 0x20)
    return 0;
  return 1;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests c_\[0-9\]*.D. -.0, 31. and -.32, 32.\[\n\r\]* into" 6 "reassoc1" } }  */
