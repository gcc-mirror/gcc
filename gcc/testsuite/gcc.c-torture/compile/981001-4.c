#define P(a,b) P1(a,b)
#define P1(a,b) a##b

#define ONCE(x, y) (x ?: (x = y()))
#define PREFIX

extern int P(PREFIX, init) (void);

int
fun(void)
{
  static int memo;
  return ONCE(memo, P(PREFIX, init));
}
