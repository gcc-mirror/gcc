void abort (void);
int ii;
typedef struct {} raw_spinlock_t;
typedef struct {
  raw_spinlock_t raw_lock;
} spinlock_t;
raw_spinlock_t one_raw_spinlock (void)
{
  raw_spinlock_t raw_lock;
  ii++;
  return raw_lock;
}
int main(void)
{
  spinlock_t lock = (spinlock_t) { .raw_lock = one_raw_spinlock() };
  if (ii != 1)
    abort ();
  return 0;
}

