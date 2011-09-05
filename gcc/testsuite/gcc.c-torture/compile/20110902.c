static inline __attribute__((always_inline)) int f (unsigned int n, unsigned int size)
{
 return (__builtin_constant_p (size != 0 && n > ~0 / size)
         ? !!(size != 0 && n > ~0 / size)
         : ({ static unsigned int count[2] = { 0, 0 };
              int r = !!(size != 0 && n > ~0 / size);
              count[r]++;
              r; }));
}

int g (unsigned int size)
{
 return f (size / 4096, 4);
}
