/* { dg-do assemble } */
/* { dg-options "-fstack-protector-strong -O1 -frename-registers" } */
/* { dg-require-effective-target fstack_protector } */

typedef unsigned int uint32_t;
struct ctx
{
  uint32_t A;
};

void *
buffer_copy (const struct ctx *ctx, void *resbuf)
{
  uint32_t buffer[4];
  buffer[0] = (ctx->A);
  __builtin_memcpy (resbuf, buffer, sizeof (buffer));
  return resbuf;
}
