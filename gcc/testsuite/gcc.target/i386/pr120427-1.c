/* { dg-do compile } */
/* { dg-options "-O2 -mtune=sapphirerapids" } */
/* { dg-final { scan-assembler-not "and\[lq\]?\[\\t \]+\\\$0, \[0-9\]*\\(" } } */

struct __pthread_mutex_s
{
  int __lock;
  unsigned int __count;
  int __owner;
  unsigned int __nusers;
  int __kind;
  short __spins;
  short __elision;
  void *p[2];
};
typedef union
{
  struct __pthread_mutex_s __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;
typedef struct { pthread_mutex_t mutex; } __rtld_lock_recursive_t;
void
foo (__rtld_lock_recursive_t *lock, int i)
{
  lock[i] = (__rtld_lock_recursive_t) {{ { 0, 0, 0, 0, 1,
      0, 0, { ((void *)0) , ((void *)0) } } }};
}
