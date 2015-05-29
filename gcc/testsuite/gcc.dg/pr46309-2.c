/* PR tree-optimization/46309 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-reassoc-details" } */

int foo (void);

void
f1 (int a)
{
  _Bool v1 = (a == 3);
  _Bool v2 = (a == 1);
  _Bool v3 = (a == 4);
  _Bool v4 = (a == 2);
  if (v1 || v2 || v3 || v4)
    foo ();
}

void
f2 (int a)
{
  _Bool v1 = (a == 1);
  _Bool v2 = (a == 2);
  _Bool v3 = (a == 3);
  _Bool v4 = (a == 4);
  if (v1 || v2 || v3 || v4)
    foo ();
}

void
f3 (unsigned int a)
{
  _Bool v1 = (a <= 31);
  _Bool v2 = (a >= 64 && a <= 95);
  _Bool v3 = (a >= 128 && a <= 159);
  _Bool v4 = (a >= 192 && a <= 223);
  if (v1 || v2 || v3 || v4)
    foo ();
}

void
f4 (int a)
{
  _Bool v1 = (a == 3);
  _Bool v2 = (a == 1);
  _Bool v3 = (a == 4);
  _Bool v4 = (a == 2);
  _Bool v5 = (a == 7);
  _Bool v6 = (a == 5);
  _Bool v7 = (a == 8);
  _Bool v8 = (a == 6);
  if (v1 || v2 || v3 || v4 || v5 || v6 || v7 || v8)
    foo ();
}

void
f5 (int a)
{
  _Bool v1 = (a != 3);
  _Bool v2 = (a != 1);
  _Bool v3 = (a != 4);
  _Bool v4 = (a != 2);
  _Bool v5 = (a != 7);
  _Bool v6 = (a != 5);
  _Bool v7 = (a != 8);
  _Bool v8 = (a != 6);
  if (v1 && v2 && v3 && v4 && v5 && v6 && v7 && v8)
    foo ();
}

void
f6 (int a)
{
  _Bool v1 = (a != 3);
  _Bool v2 = (a != 1);
  _Bool v3 = (a != 4);
  _Bool v4 = (a != 2);
  _Bool v5 = (a != 7);
  _Bool v6 = (a != 5);
  _Bool v7 = (a != 8);
  _Bool v8 = (a != 6);
  if ((v1 && v2 && v3 && v4) && (v5 && v6 && v7 && v8))
    foo ();
}

int
f7 (int a)
{
  _Bool v1 = (a == 3);
  _Bool v2 = (a == 1);
  _Bool v3 = (a == 4);
  _Bool v4 = (a == 2);
  _Bool v5 = (a == 7);
  _Bool v6 = (a == 5);
  _Bool v7 = (a == 8);
  _Bool v8 = (a == 6);
  return v1 || v2 || v3 || v4 || v5 || v6 || v7 || v8;
}

_Bool
f8 (int a)
{
  _Bool v1 = (a == 3);
  _Bool v2 = (a == 1);
  _Bool v3 = (a == 4);
  _Bool v4 = (a == 2);
  _Bool v5 = (a == 7);
  _Bool v6 = (a == 5);
  _Bool v7 = (a == 8);
  _Bool v8 = (a == 6);
  return v1 || v2 || v3 || v4 || v5 || v6 || v7 || v8;
}

int
f9 (int a)
{
  _Bool v1 = (a != 3);
  _Bool v2 = (a != 1);
  _Bool v3 = (a != 4);
  _Bool v4 = (a != 2);
  _Bool v5 = (a != 7);
  _Bool v6 = (a != 5);
  _Bool v7 = (a != 8);
  _Bool v8 = (a != 6);
  return v1 && v2 && v3 && v4 && v5 && v6 && v7 && v8;
}

_Bool
f10 (int a)
{
  _Bool v1 = (a != 3);
  _Bool v2 = (a != 1);
  _Bool v3 = (a != 4);
  _Bool v4 = (a != 2);
  _Bool v5 = (a != 7);
  _Bool v6 = (a != 5);
  _Bool v7 = (a != 8);
  _Bool v8 = (a != 6);
  return v1 && v2 && v3 && v4 && v5 && v6 && v7 && v8;
}

/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.1, 1. and -.2, 2. and -.3, 3. and -.4, 4.\[\n\r\]* into" 2 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.0, 31. and -.64, 95.\[\n\r\]* into" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.128, 159. and -.192, 223.\[\n\r\]* into" 1 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests a_\[0-9\]*.D. -.1, 1. and -.2, 2. and -.3, 3. and -.4, 4. and -.5, 5. and -.6, 6. and -.7, 7. and -.8, 8.\[\n\r\]* into" 7 "reassoc1" } } */
/* { dg-final { scan-tree-dump-times "Optimizing range tests \[^\r\n\]*_\[0-9\]* -.0, 31. and -.128, 159.\[\n\r\]* into" 1 "reassoc2" } } */
