extern void callmesecond();
static void callmealias() __attribute__((weakref ("callmesecond")));

void
b()
{
  callmealias();
}
