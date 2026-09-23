/* Verify that function return values are const 0 and 1 and not reg.
   Extracted from BPF selftest sockopt_multi.c  */

/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v4" } */

struct bpf_sockopt {
   int level, optname, optlen, retval;
   void *optval_end, *optval;
};

typedef unsigned char u8;

int getsockopt_child(struct bpf_sockopt *ctx)
{
 u8 *optval_end = ctx->optval_end;
 u8 *optval = ctx->optval;

//  if (ctx->optname != 1)
//    goto out;
 
 if (ctx->level != 0 || ctx->optname != 1)
  goto out;

 if (optval[0] != 0x80)
  return 0;

 ctx->retval = 0;
 return 1;

out:
 return 1;
}

/* { dg-final { scan-assembler-not   {\*\(u32 \*\) \(r.\+12\) = r.} } } */
/* { dg-final { scan-assembler-times {\*\(u32 \*\) \(r.\+12\) = 0} 1 } } */
