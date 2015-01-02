/* { dg-shouldfail "tsan" } */
#include <pthread.h>
#include <stdio.h>
#include <stdint.h>

uint64_t Global[2];

void *Thread1(void *x) {
  Global[1]++;
  return NULL;
}

void *Thread2(void *x) {
  char *p1 = reinterpret_cast<char *>(&Global[0]);
  struct __attribute__((packed, aligned(1))) u_uint64_t { uint64_t val; };
  u_uint64_t *p4 = reinterpret_cast<u_uint64_t *>(p1 + 1);
  (*p4).val++;
  return NULL;
}

int main() {
  pthread_t t[2];
  pthread_create(&t[0], NULL, Thread1, NULL);
  pthread_create(&t[1], NULL, Thread2, NULL);
  pthread_join(t[0], NULL);
  pthread_join(t[1], NULL);
  printf("Pass\n");
  /* { dg-output "WARNING: ThreadSanitizer: data race.*(\n|\r\n|\r)" } */
  /* { dg-output "Pass.*" } */
  return 0;
}
