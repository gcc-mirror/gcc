/* Ensure memcpy is expanded inline rather than emitting a libcall.  */

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
cpy_1(struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest;
  dest = data;
  dest += 16;

  __builtin_memcpy (dest, data, 8);
}

/* { dg-final { scan-assembler-times "call" 0 } } */
