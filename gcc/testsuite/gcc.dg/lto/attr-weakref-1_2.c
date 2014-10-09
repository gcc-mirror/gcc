extern void callmefirst();
static void callmealias() __attribute__((weakref ("callmefirst")));

void
c()
{
  callmealias();
}
