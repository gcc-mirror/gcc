/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O -frerun-cse-after-loop -fschedule-insns2 -fselective-scheduling2 -fno-tree-ch -funroll-loops --param=max-sched-extend-regions-iters=2 --param=max-sched-region-blocks=15" } */
extern void *memcpy(void *dest, const void *src, __SIZE_TYPE__ n);

void bar (void *, void *, void *);

void foo
  (void *p, char *data, unsigned data_len)
{
  int buffer[8];
  int buf2[8];
  unsigned i;
  for (i = 0; i + 8 <= data_len; i += 8)
    bar (p, buffer, data + i);
  memcpy (buf2, data + i, data_len);
}
