/* Test that we error if memmove cannot be expanded inline.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int __u32;

struct context {
 unsigned int data;
 unsigned int data_end;
 unsigned int data_meta;
};

void
mov_2_unsupported (struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest;
  dest = data;
  dest += 4;

  __builtin_memmove (dest, data, ctx->data_meta); /* { dg-error "could not inline call" } */
}
