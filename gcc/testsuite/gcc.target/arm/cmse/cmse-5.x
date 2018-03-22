extern float bar (void);

float __attribute__ ((cmse_nonsecure_entry))
foo (void)
{
  return bar ();
}
