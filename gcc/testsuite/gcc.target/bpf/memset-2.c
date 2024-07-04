/* Test that we error if memset cannot be expanded inline.  */

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
set_variable (struct context *ctx)
{
  void *data = (void *)(long)ctx->data;
  char *dest = data;
  __builtin_memset (dest, 0xbc, ctx->data_meta); /* { dg-error "could not inline call" } */
}
