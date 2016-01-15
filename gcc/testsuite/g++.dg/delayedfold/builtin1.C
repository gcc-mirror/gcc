// PR c++/68847
// { dg-do compile { target cas_int } }

class RegionLock {
  template <unsigned long> void m_fn1();
  int spinlock;
} acquire_zero;
int acquire_one;
template <unsigned long> void RegionLock::m_fn1() {
  __atomic_compare_exchange(&spinlock, &acquire_zero, &acquire_one, false, 2, 2);
}
