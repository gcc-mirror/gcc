/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -fpic" } */

typedef struct {
  int lock;
  int pad0_;
} mutex_t;

static mutex_t main_arena;

void __malloc_check_init()
{
  for(;;)
    __asm__ __volatile__ ("": "+m"(main_arena.lock) );
}

