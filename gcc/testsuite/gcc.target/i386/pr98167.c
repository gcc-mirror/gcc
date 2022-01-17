/* PR target/98167 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

/* { dg-final { scan-assembler-times "vpshufd\t" 8 } } */
/* { dg-final { scan-assembler-times "vpermilps\t" 3 } } */

#define VEC_PERM_4 \
  2, 3, 1, 0
#define VEC_PERM_8 \
  4, 5, 6, 7, 3, 2, 1, 0
#define VEC_PERM_16 \
  8, 9, 10, 11, 12, 13, 14, 15, 7, 6, 5, 4, 3, 2, 1, 0

#define TYPE_PERM_OP(type, size, op, name) \
  typedef type v##size##s##type __attribute__ ((vector_size(4*size))); \
  v##size##s##type type##foo##size##i_##name (v##size##s##type a, \
					      v##size##s##type b) \
  { \
    v##size##s##type a1 = __builtin_shufflevector (a, a, \
						   VEC_PERM_##size); \
    v##size##s##type b1 = __builtin_shufflevector (b, b, \
						   VEC_PERM_##size); \
    return a1 op b1; \
  }

#define INT_PERMS(op, name) \
  TYPE_PERM_OP (int, 4, op, name) \

#define FP_PERMS(op, name) \
  TYPE_PERM_OP (float, 4, op, name) \

INT_PERMS (+, add)
INT_PERMS (-, sub)
INT_PERMS (*, mul)
INT_PERMS (|, ior)
INT_PERMS (^, xor)
INT_PERMS (&, and)
INT_PERMS (<<, shl)
INT_PERMS (>>, shr)
FP_PERMS (+, add)
FP_PERMS (-, sub)
FP_PERMS (*, mul)

