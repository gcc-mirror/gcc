// PR c++/67064
// { dg-options "-w" }

struct s {
  int i;
};

register struct s *reg __asm__( "1" );

int f(void)
{
  int i;

  i = reg->i;
  i = (reg)->i;

  return i;
}
