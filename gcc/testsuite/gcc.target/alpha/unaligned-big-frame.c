/* Unaligned accesses to a stack object whose byte address does not fit
   add_operand used to produce an unrecognizable insn.  A stack protector
   canary pushes the offsets of a 65529-byte array past the -65536 boundary
   that constraint L still accepts.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-strong -msafe-partial" } */
/* { dg-require-effective-target fstack_protector } */

struct __attribute__((packed)) S2 { short x; };
struct __attribute__((packed)) U2 { unsigned short x; };
struct __attribute__((packed)) U8 { unsigned long x; };

void g (long);
void sink (char *);

/* alpha_expand_unaligned_load, sign && size == 2.  */
void
load_signed_hi (void)
{
  char a[65529];
  g (((struct S2 *) a)->x);
}

/* alpha_expand_unaligned_load, general case.  */
void
load_unsigned_hi (void)
{
  char a[65529];
  g (((struct U2 *) a)->x);
}

void
load_di (void)
{
  char a[65529];
  g (((struct U8 *) a)->x);
}

/* alpha_expand_unaligned_store_safe_partial, both the high and the low
   address.  */
void
store_di (unsigned long v)
{
  char a[65529];
  ((struct U8 *) a)->x = v;
  sink (a);
}
