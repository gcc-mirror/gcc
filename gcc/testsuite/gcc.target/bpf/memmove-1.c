/* Ensure memmove is expanded inline rather than emitting a libcall.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

struct context {
 unsigned int data;
 unsigned int data_end;
 unsigned int data_meta;
 unsigned int ingress;
 unsigned int queue_index;
 unsigned int egress;
};

void
mov_1_nooverlap (struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest;
  dest = data;
  dest += 16;

  __builtin_memmove (dest, data, 12);
}

void
mov_1_overlap (struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest;
  dest = data;
  dest += 4;

  __builtin_memmove (dest, data, 12);
}

void
mov_1_arbitrary (struct context *ctx_a, struct context *ctx_b)
{
  void *src = (void *)(long)ctx_a->data;
  void *dst = (void *)(long)ctx_b->data;

  __builtin_memmove (dst, src, 12);
}

/* { dg-final { scan-assembler-times "call" 0 } } */
