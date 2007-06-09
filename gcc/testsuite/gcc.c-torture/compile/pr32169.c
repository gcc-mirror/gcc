void f(char);
static inline
void * __memset_generic(char c)
{
  f(c);
}
int prepare_startup_playback_urb(
     int b,
     int c
)
{
  char d;
  if (b)
    __memset_generic(c == ( 1) ? 0x80 : 0);
  else
    __memset_generic (c == ( 1) ? 0x80 : 0);
}
