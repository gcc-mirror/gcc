/* No stack red zone.  PR38644.  */
/* { dg-skip-if "incompatible options" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-mthumb -O2" } */
/* { dg-final { scan-assembler "ldrb\[^\n\]*\\n\[\t \]*add\[\t \]*sp" { target { ! arm*-*-uclinuxfdpiceabi } } } } */

extern int doStreamReadBlock (int *, char *, int size, int);

char readStream (int *s)
{
       char c = 0;
       doStreamReadBlock (s, &c, 1, *s);
       return c;
}
