/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#define NULL ((void *)0)

extern int printf (const char *__restrict __format, ...);
extern int vasprintf (char **__restrict __ptr, const char *__restrict __f,
		      __builtin_va_list __arg)
  __attribute__ ((__nothrow__, __format__ (__printf__, 2, 0))) ;
extern void free (void *__ptr) __attribute__ ((__nothrow__ , __leaf__));

static char * __attribute__ ((__format__ (__printf__, 1, 2)))
zasprintf (const char *format, ...)
{
  char *resultp;
  __builtin_va_list args;
  __builtin_va_start (args, format);
  int r = vasprintf (&resultp, format, args);
  __builtin_va_end (args);
  return r < 0 ? NULL : resultp;
}

int run_test() {
    char *buf = NULL;
    char *bar = NULL;
    char *baz = NULL;
    int i = 1232;

    printf("static function check\n");

    buf = zasprintf("i = %d", i);
    if (buf) {
        printf("buf = %s\nbuf = %p\n", buf, buf);
    }

    bar = zasprintf("i = %d - %d", i, i - 13);
    if (bar) {
        printf("bar = %s\nbar = %p\n", bar, bar);
        printf("buf = %s\nbuf = %p\n", buf, buf);
    }

    baz = zasprintf("No i's here");
    if (baz) {
        printf("baz = %s\nbaz = %p\n", baz, baz);
        printf("bar = %s\nbar = %p\n", bar, bar);
        printf("buf = %s\nbuf = %p\n", buf, buf);
    }

    free(buf);
    free(bar);
    free(baz);

    return 1;
}

int main(int argc, char **argv) {
    return run_test();
}
