/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcf-protection=branch -mcet-switch -fno-pie -fno-pic" } */
/* { dg-final { scan-assembler-times "endbr32" 9 { target ia32 } } } */
/* { dg-final { scan-assembler-times "endbr64" 9 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\[ \t]+jmp\[ \t]+\[*]" 2 } } */

/* Both tables reach the same eight destinations.  */

void f0 (void);
void f1 (void);
void f2 (void);
void f3 (void);
void f4 (void);
void f5 (void);
void f6 (void);
void f7 (void);

void
foo (unsigned x, int which)
{
  if (which)
    switch (x)
      {
      case 0: goto a;
      case 1: goto b;
      case 2: goto c;
      case 3: goto d;
      case 4: goto e;
      case 5: goto f;
      case 6: goto g;
      case 7: goto h;
      default: return;
      }
  else
    switch (x)
      {
      case 0: goto h;
      case 1: goto g;
      case 2: goto f;
      case 3: goto e;
      case 4: goto d;
      case 5: goto c;
      case 6: goto b;
      case 7: goto a;
      default: return;
      }
a: f0 (); return;
b: f1 (); return;
c: f2 (); return;
d: f3 (); return;
e: f4 (); return;
f: f5 (); return;
g: f6 (); return;
h: f7 (); return;
}
