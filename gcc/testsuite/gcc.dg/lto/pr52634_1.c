int cfliteKeyCallBacks = 5;
extern int cfliteValueCallBacks __attribute__((alias("cfliteKeyCallBacks")));
void baz(void *ptr)
{
  asm volatile (""::"r"(ptr));
}
