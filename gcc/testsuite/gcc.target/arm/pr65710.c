/* { dg-do compile } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } {"-mfloat-abi=soft" } } */
/* { dg-options "-mthumb -O2 -mfloat-abi=soft -w -fpermissive" } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */

struct ST {
  char *buffer;
  int used;
};

struct ST *h;

enum { no_op, duplicate, pop_failure_jump, dummy_failure_jump };

typedef struct {
  unsigned pointer;
} byte_fail_stack_elt_t;

typedef struct { unsigned avail; } byte_fail_stack_type;

typedef union {
  byte_fail_stack_elt_t word;
  struct {
    unsigned match_null_string_p : 2;
    unsigned is_active : 1;
    unsigned ever_matched_something : 1;
  } bits;
} byte_register_info_type;

static int a;
int b = 0;
int c, e, f;
int *d, *g;

int
byte_re_match_2_internal_size2(const int p2, int p3, const int p4) {
  int i, p;
  char *j;
  char k, l, m, n = h;
  byte_fail_stack_type o;
  byte_fail_stack_elt_t *q;
  unsigned int s = (unsigned int)h;
  long t, u;
  char **v, *w, **x, **y, **t1;
  byte_register_info_type *z, *t2 = __builtin_alloca(s);
  x = __builtin_alloca(s);
  y = __builtin_alloca(s);
  z = __builtin_alloca(sizeof(byte_register_info_type));
  k = p4 + byte_re_match_2_internal_size2;
  if (p3)
    f = p4;
  for (;;) {
    if (h == h->used) {
      g = f;
      if (o.avail) {
        b = 1;
        for (; i < s; i++)
          t1[i] = w;
        goto fail;
      }
      e = 30 > s;
      d = p4;
      d[s] = 1;
      return;
    }
    switch (*h->buffer++) {
    case no_op:
      while (m && n ?: *g)
        ;
      y[*h->buffer] = z[*h->buffer].bits.match_null_string_p ? w == &a ?: w : w;
      w = g;
      if (t) {
        char r = h;
        while (r && z[r].bits.is_active)
          r--;
        if (r == 0)
          ;
        else
          u = r;
      }
      switch (*j++)
      case dummy_failure_jump:
      i = j;
      if (i)
        if (z[*h->buffer].bits.ever_matched_something) {
          unsigned r;
          z[*h->buffer].bits.ever_matched_something = r = *h->buffer;
          for (; r + *(h->buffer + 1); r++) {
            v = x[r];
            w[r] = y[r];
          }
        }
      break;
    case duplicate: {
      char *t3 = p2 + p3;
      if (t3)
        break;
    }
      if ((p3 ?: p4) == k)
        goto fail;
    case pop_failure_jump:
      for (; c; c--)
        t2[c].word = q[o.avail];
      char t4;
      q = t4 = __builtin_allocamemcpy(t4 ?: (p <<= 1));
    }
    continue;
  fail : {
    unsigned t5;
    t = q;
    t5 = u;
    for (; t5 >= t; t5--)
      v[t5] = q[--o.avail].pointer;
    switch (*h->buffer)
    case pop_failure_jump:
    goto fail;
  }
    m = &l;
  }
}
