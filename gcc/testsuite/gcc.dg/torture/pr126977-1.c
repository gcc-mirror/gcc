/* { dg-do compile } */
/* PR tree-optimization/126977 */

/* All of these should be valid and should not cause
   any errors even with factoring happening. */

typedef __UINT32_TYPE__ u32;

u32 crc32_data32 (u32 x, u32 y, int z)
{
  if (z)
    return __builtin_crc32_data32 (x, y, 0x4002123);
  else
    return __builtin_crc32_data32 (x, y, 0x4002124);
}

int bos (int z, void *p)
{
  if (z)
    return __builtin_object_size (p, 0);
  else
    return __builtin_object_size (p, 1);
}


void prefetch (unsigned *x, int z)
{
  if (z)
    __builtin_prefetch (x, 1);
  else
    __builtin_prefetch (x, 0);
}
void *faddr (unsigned *x, int z)
{
  if (z)
    return __builtin_frame_address (1);
  else
    return __builtin_frame_address (0);
}


int expect_issue (long a, int z)
{
  if (z)
    return __builtin_expect (a, 1);
  else
    return __builtin_expect (a, 0);
}


struct f
{
  int a[2];
};
struct f1
{
  char a[2];
};
int islock (long a, int z, void *p)
{
  if (z)
    return __atomic_is_lock_free (sizeof(struct f), p);
  else
    return __atomic_is_lock_free (sizeof(struct f1), p);
}


int clz1 (int z, unsigned int p)
{
  if (z)
    return __builtin_clzg (p, 0);
  else
    return __builtin_clzg (p, 32);
}
