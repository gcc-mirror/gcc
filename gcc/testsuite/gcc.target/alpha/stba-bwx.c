/* { dg-do compile } */
/* { dg-options "-mbwx" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

typedef union
  {
    int i;
    char c;
  }
char_a;

void
stba (char_a *p, char v)
{
  p->c = v;
}

/* Expect assembly such as:

	stb $17,0($16)
 */

/* { dg-final { scan-assembler-times "\\sstb\\s\\\$17,0\\\(\\\$16\\\)\\s" 1 } } */
