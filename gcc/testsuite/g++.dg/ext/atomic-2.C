// PR c++/57926

long Mutex[1];

int AcquireLogMutex(void)
{
  return __atomic_exchange_n(Mutex, 1, __ATOMIC_SEQ_CST);
}

void ReleaseLogMutex(void)
{
  long i = 0;
  __atomic_store(Mutex, &i, __ATOMIC_SEQ_CST);
}
