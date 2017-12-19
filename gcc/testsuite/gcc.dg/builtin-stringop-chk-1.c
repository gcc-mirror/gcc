/* Test whether buffer overflow warnings for __*_chk builtins
   are emitted properly.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-format -std=gnu99 -ftrack-macro-expansion=0" } */
// { dg-skip-if "packed attribute missing for t" { "epiphany-*-*" } }

extern void abort (void);

#include "../gcc.c-torture/execute/builtins/chk.h"

#define va_list    __builtin_va_list
#define va_start   __builtin_va_start
#define va_end     __builtin_va_end

volatile void *vx;
char buf1[20];
int x;

void
test (int arg, ...)
{
  char buf2[20];
  va_list ap;
  char *p = &buf1[10], *q;

  memcpy (&buf2[19], "ab", 1);
  memcpy (&buf2[19], "ab", 2); /* { dg-warning "writing 2 bytes into a region of size 1" "memcpy" } */
  vx = mempcpy (&buf2[19], "ab", 1);
  vx = mempcpy (&buf2[19], "ab", 2); /* { dg-warning "writing 2 " "mempcpy" } */
  memmove (&buf2[18], &buf1[10], 2);
  memmove (&buf2[18], &buf1[10], 3); /* { dg-warning "writing 3 " "memmove" } */
  memset (&buf2[16], 'a', 4);
  memset (&buf2[15], 'b', 6); /* { dg-warning "writing 6 " "memset" } */
  strcpy (&buf2[18], "a");
  strcpy (&buf2[18], "ab"); /* { dg-warning "writing 3 " "strcpy" } */
  vx = stpcpy (&buf2[18], "a");
  vx = stpcpy (&buf2[18], "ab"); /* { dg-warning "writing 3" "stpcpy" } */
  strncpy (&buf2[18], "a", 2);

  /* Both warnings below are equally meaningful.  */
  strncpy (&buf2[18], "a", 3); /* { dg-warning "(writing 3 bytes into a region of size 2|specified bound 3 exceeds destination size 2)" "strncpy" } */

  strncpy (&buf2[18], "abc", 2);
  strncpy (&buf2[18], "abc", 3); /* { dg-warning "writing 3 " "strncpy" } */
  memset (buf2, '\0', sizeof (buf2));
  strcat (&buf2[18], "a");
  memset (buf2, '\0', sizeof (buf2));
  strcat (&buf2[18], "ab"); /* { dg-warning "writing 3 " "strcat" } */
  sprintf (&buf2[18], "%s", buf1);
  sprintf (&buf2[18], "%s", "a");
  sprintf (&buf2[18], "%s", "ab"); /* { dg-warning "writing 3 " "sprintf" } */
  sprintf (&buf2[18], "a");
  sprintf (&buf2[18], "ab"); /* { dg-warning "writing 3 " "sprintf" } */
  snprintf (&buf2[18], 2, "%d", x);
  /* N argument to snprintf is the size of the buffer.
     Although this particular call wouldn't overflow buf2,
     incorrect buffer size was passed to it and therefore
     we want a warning and runtime failure.  */
  snprintf (&buf2[18], 3, "%d", x); /* { dg-warning "specified bound 3 exceeds destination size 2" "snprintf" } */
  va_start (ap, arg);
  vsprintf (&buf2[18], "a", ap);
  va_end (ap);

  va_start (ap, arg);
  vsprintf (&buf2[18], "ab", ap); /* { dg-warning "writing 3" "vsprintf" } */
  va_end (ap);
  va_start (ap, arg);
  vsnprintf (&buf2[18], 2, "%s", ap);
  va_end (ap);
  va_start (ap, arg);
  /* See snprintf above.  */
  vsnprintf (&buf2[18], 3, "%s", ap); /* { dg-warning "specified bound 3 exceeds destination size 2" "vsnprintf" } */
  va_end (ap);

  p = p + 10;
  memset (p, 'd', 0);
  q = strcpy (p, ""); /* { dg-warning "writing 1 " "strcpy" } */

  /* This invokes undefined behavior, since we are past the end of buf1.  */
  p = p + 10;
  memset (p, 'd', 1); /* { dg-warning "writing 1 " "memset" } */

  memset (q, 'd', 0);
  memset (q, 'd', 1); /* { dg-warning "writing 1 " "memset" } */
  q = q - 10;
  memset (q, 'd', 10);
}

char *str = "ABCDEFG";
typedef struct { char b[16]; } H;

/* Some brown paper bag bugs found in real applications.
   This test is here merely for amusement.  */

void
test2 (const H h)
{
  char c;
  strncpy (&c, str, 3); /* { dg-warning "(writing 3 bytes into a region of size 1|specified bound 3 exceeds destination size 1)" "strncpy" } */

  struct { char b[4]; } x;
  sprintf (x.b, "%s", "ABCD"); /* { dg-warning "writing 5" "sprintf" } */

  unsigned int i;
  memcpy (&i, &h, sizeof (h)); /* { dg-warning "writing 16 " "memcpy" } */

  unsigned char buf[21];
  memset (buf + 16, 0, 8); /* { dg-warning "writing 8 " "memset" } */

  typedef struct { __INT32_TYPE__ i, j, k, l; } S;
  S *s[3];
  memset (s, 0, sizeof (S) * 3); /* { dg-warning "writing 48 " "memset" } */

  struct T { char a[8]; char b[4]; char c[10]; } t;
  stpcpy (t.c,"Testing..."); /* { dg-warning "writing" "stpcpy" } */

  char b1[7];
  char b2[4];
  memset (b1, 0, sizeof (b1));
  memset (b2, 0, sizeof (b1)); /* { dg-warning "writing 7" "memset" } */
}
