// { dg-do run { target pthread } }
// { dg-options "-lasan -lpthread" }
// { dg-shouldfail "asan" }

#include <pthread.h>

int *x;

void *AllocThread(void *arg) {
  x = new int;
  *x = 42;
  return NULL;
}

void *FreeThread(void *arg) {
  delete x;
  return NULL;
}

void *AccessThread(void *arg) {
  *x = 43;  // BOOM
  return NULL;
}

typedef void* (*callback_type)(void* arg);

void *RunnerThread(void *function) {
  pthread_t thread;
  pthread_create(&thread, NULL, (callback_type)function, NULL);
  pthread_join(thread, NULL);
  return NULL;
}

void RunThread(callback_type function) {
  pthread_t runner;
  pthread_create(&runner, NULL, RunnerThread, (void*)function);
  pthread_join(runner, NULL);
}

int main(int argc, char *argv[]) {
  RunThread(AllocThread);
  RunThread(FreeThread);
  RunThread(AccessThread);
  return (x != 0);
}

// { dg-output "ERROR: AddressSanitizer: heap-use-after-free.*(\n|\r\n|\r)" }
// { dg-output "WRITE of size 4 at 0x\[0-9a-f\]+ thread T(\[0-9\]+).*(\n|\r\n|\r)" }
// { dg-output "freed by thread T(\[0-9\]+) here:.*(\n|\r\n|\r)" }
// { dg-output "previously allocated by thread T(\[0-9\]+) here:.*(\n|\r\n|\r)" }
// { dg-output "Thread T\\2 created by T(\[0-9\]+) here:.*(\n|\r\n|\r)" }
// { dg-output "Thread T\\8 created by T0 here:.*(\n|\r\n|\r)" }
// { dg-output "Thread T\\4 created by T(\[0-9\]+) here:.*(\n|\r\n|\r)" }
// { dg-output "Thread T\\11 created by T0 here:.*(\n|\r\n|\r)" }
// { dg-output "Thread T\\6 created by T(\[0-9\]+) here:.*(\n|\r\n|\r)" }
// { dg-output "Thread T\\14 created by T0 here:" }
