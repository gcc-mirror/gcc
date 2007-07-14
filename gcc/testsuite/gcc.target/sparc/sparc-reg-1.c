/* PR middle-end/20263 */

/* { dg-do assemble } */
/* { dg-options "" } */

register void *tp __asm__("%g7");

void set_tp(void)
{
  tp = 0;
} 
