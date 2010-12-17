/* PR target/45670 */
/* { dg-do compile } */
/* { dg-options "-Os -mtune=generic" } */

struct S
{
  float *buf;
  int size;
};

void
foo (struct S *s)
{
  int i;
  for (i = 0; i < s->size; ++i)
    s->buf[i] = 0;
}

/* Ensure we don't generate
   lea (reg1,4),reg2; add (reg3),reg2; movl $0, (reg2)
   instead of smaller
   mov (reg3),reg2; movl $0, (reg2,reg1,4)  */
/* { dg-final { scan-assembler-not "lea\[lq\]" } } */
