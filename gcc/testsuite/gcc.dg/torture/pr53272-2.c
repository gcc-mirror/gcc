__attribute__ ((__noinline__, __noclone__))
int foo(void *x)
{
  asm ("");
  return *(int *) x != 42;
}

__attribute__ ((__noinline__, __noclone__))
void foobar(void *x)
{
  asm ("");
  if (foo(x))
    __builtin_abort();
}

struct rtc_class_ops {
 int (*f)(void *, unsigned int enabled);
};

struct rtc_device
{
 void *owner;
 struct rtc_class_ops *ops;
 int ops_lock;
};

extern __attribute__ ((__noinline__, __noclone__))
int rtc_update_irq_enable(struct rtc_device *rtc, unsigned int);

int main(void)
{
 struct rtc_class_ops ops = {(void *) 0};
  struct rtc_device dev1 = {0, &ops, 42};

  if (rtc_update_irq_enable (&dev1, 1) != -22)
    __builtin_abort ();

  __builtin_exit (0);
}
