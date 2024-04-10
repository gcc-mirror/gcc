/* Ensure memset is expanded inline rather than emitting a libcall.  */

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
set_small (struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest = data;
  __builtin_memset (dest + 4, 0, sizeof (struct context) - 4);
}

void
set_large (struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest = data;
  __builtin_memset (dest, 0xfe, 130);
}

/* { dg-final { scan-assembler-times "call" 0 } } */
