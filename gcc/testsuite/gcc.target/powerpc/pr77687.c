/* { dg-options "-std=gnu99 -O2" } */
/* { dg-final { scan-assembler-not {\mmr r?1,r?11\M.*11.*\mblr\M} } } */

/* PR77687: We used to do stack accesses (via r11) after restoring r1.  */

void g(int, char *);
const char * dum = "hello";

void f(int x)
{
       char big[200000];
 start:
       g(x, big);
       g(x, big);
       register void *p asm("r11") = &&start;
       asm("" : : "r"(p));
       asm("" : : :"r28");
       asm("" : : :"r29");
       asm("" : : :"r30");
}
