/* { dg-shouldfail "tsan" } */

#include <pthread.h>
#include <unistd.h>

struct bitfield
{
  int a:10;
  int b:10;
} Global;

void *Thread1(void *x) {
  sleep(1);
  Global.a = 42;
  return x;
}

int main() {
  pthread_t t;
  pthread_create(&t, 0, Thread1, 0);
  Global.b = 43;
  pthread_join(t, 0);
  return Global.a;
}

/* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
