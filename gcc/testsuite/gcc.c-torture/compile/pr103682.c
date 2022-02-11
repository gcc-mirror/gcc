int bug(unsigned *ready, unsigned u) {
  return __atomic_fetch_and (ready, ~u, 0) & u;
}
