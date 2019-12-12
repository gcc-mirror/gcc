/* { dg-do run } */
/* { dg-require-effective-target alloca } */

/* This testcase checks that allocas and VLAs inside loop are correctly unpoisoned.  */

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "sanitizer/asan_interface.h"

void *top, *bot;
volatile int thirty_two = 32;

__attribute__((noinline)) void foo(int len) {
  char x;
  top = &x;
  volatile char array[len];
  assert(!((uintptr_t) array & 31L));
  void *p = __builtin_alloca(len);
  for (int i = 0; i < thirty_two; ++i) {
    char array[i];
    bot = array;
    /* Just to prevent optimization.  */
    printf("%p\n", bot);
    assert(!((uintptr_t) bot & 31L));
  }
}

int main(int argc, char **argv) {
  foo(thirty_two);
  void *q = __asan_region_is_poisoned(bot, (char *)top - (char *)bot);
  assert(!q);
  return 0;
}
