/* Make sure that the H8 backend does not generate a div
   instruction in a delay slot. */
/* { dg-options "-Os" } */
/* { dg-skip-if "" { "h8300*-*-*" } "*" "-msx*" }  */
/* { dg-final { scan-assembler-not "\tbra/s\t.*\n\tdiv*" } } */

extern volatile unsigned long timer_ticks;
#define timer_ms_elapsed(ticks) (((unsigned long)(timer_ticks-ticks))/10)
unsigned long ticks;

unsigned tst_read( unsigned char idx )
{
        switch( idx )
        {
                case 0x62: return timer_ms_elapsed(ticks);
                case 0x61: return timer_ticks;
                default: return 0;
        }
}
