extern volatile int ReadyFlag_NotProperlyInitialized;

volatile int ReadyFlag_NotProperlyInitialized=1;

int main(void)
{
  if (ReadyFlag_NotProperlyInitialized != 1)
    __builtin_abort ();
  return 0;
}
