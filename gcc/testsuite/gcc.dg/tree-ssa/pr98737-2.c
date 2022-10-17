/* PR target/98737 */
/* { dg-do compile { target i?86-*-* x86_64-*-* powerpc*-*-* aarch64*-*-* } } */
/* { dg-options "-O2 -fdump-tree-optimized -fcompare-debug" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */
/* { dg-final { scan-tree-dump-not "__atomic_\[^f]" "optimized" } } */
/* { dg-final { scan-tree-dump-not "__sync_\[^f]" "optimized" } } */

typedef signed char schar;
typedef unsigned long ulong;
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned char uchar;
long vlong;
int vint;
short vshort;
schar vschar;
ulong vulong;
uint vuint;
ushort vushort;
uchar vuchar;
#define A(n, t, ut, f, o, ...) \
t fn##n (t x)					\
{						\
  ut z = f (&v##t, x, ##__VA_ARGS__);		\
  t w = (t) z;					\
  return w o x;					\
}
#define B(n, f, o, ...) \
  A(n##0, long, ulong, f, o, ##__VA_ARGS__)	\
  A(n##1, int, uint, f, o, ##__VA_ARGS__)	\
  A(n##2, short, ushort, f, o, ##__VA_ARGS__)	\
  A(n##3, schar, uchar, f, o, ##__VA_ARGS__)	\
  A(n##4, ulong, ulong, f, o, ##__VA_ARGS__)	\
  A(n##5, uint, uint, f, o, ##__VA_ARGS__)	\
  A(n##6, ushort, ushort, f, o, ##__VA_ARGS__)	\
  A(n##7, uchar, uchar, f, o, ##__VA_ARGS__)

B(00, __atomic_add_fetch, -, __ATOMIC_RELAXED)
B(01, __atomic_sub_fetch, +, __ATOMIC_RELAXED)
B(03, __atomic_xor_fetch, ^, __ATOMIC_RELAXED)
B(05, __sync_add_and_fetch, -)
B(06, __sync_sub_and_fetch, +)
B(08, __sync_xor_and_fetch, ^)

#undef A
#define A(n, t, ut, f, o, ...) \
t fn##n (void)					\
{						\
  ut z = f (&v##t, 42, ##__VA_ARGS__);		\
  t w = (t) z;					\
  return w o 42;				\
}

B(10, __atomic_add_fetch, -, __ATOMIC_RELAXED)
B(11, __atomic_sub_fetch, +, __ATOMIC_RELAXED)
B(13, __atomic_xor_fetch, ^, __ATOMIC_RELAXED)
B(15, __sync_add_and_fetch, -)
B(16, __sync_sub_and_fetch, +)
B(18, __sync_xor_and_fetch, ^)

#undef A
#define A(n, t, ut, f, o, ...) \
t fn##n (t x)					\
{						\
  ut z = f (&v##t, x, ##__VA_ARGS__);		\
  t w = (t) z;					\
  t v = w o x;					\
  return v == 0;				\
}

B(20, __atomic_add_fetch, -, __ATOMIC_RELAXED)
B(21, __atomic_sub_fetch, +, __ATOMIC_RELAXED)
B(23, __atomic_xor_fetch, ^, __ATOMIC_RELAXED)
B(25, __sync_add_and_fetch, -)
B(26, __sync_sub_and_fetch, +)
B(28, __sync_xor_and_fetch, ^)

#undef A
#define A(n, t, ut, f, o, ...) \
t fn##n (void)					\
{						\
  ut z = f (&v##t, 42, ##__VA_ARGS__);		\
  t w = (t) z;					\
  t v = w o 42;					\
  return v != 0;				\
}

B(30, __atomic_add_fetch, -, __ATOMIC_RELAXED)
B(31, __atomic_sub_fetch, +, __ATOMIC_RELAXED)
B(33, __atomic_xor_fetch, ^, __ATOMIC_RELAXED)
B(35, __sync_add_and_fetch, -)
B(36, __sync_sub_and_fetch, +)
B(38, __sync_xor_and_fetch, ^)

#undef A
#define A(n, t, ut, f, o, ...) \
t fn##n (t x)					\
{						\
  return (t) (((t) f (&v##t, x, ##__VA_ARGS__))	\
	      o x) != 0;			\
}

B(40, __atomic_add_fetch, -, __ATOMIC_RELAXED)
B(41, __atomic_sub_fetch, +, __ATOMIC_RELAXED)
B(43, __atomic_xor_fetch, ^, __ATOMIC_RELAXED)
B(45, __sync_add_and_fetch, -)
B(46, __sync_sub_and_fetch, +)
B(48, __sync_xor_and_fetch, ^)

#undef A
#define A(n, t, ut, f, o, ...) \
t fn##n (void)					\
{						\
  return (t) (((t) f (&v##t, 42, ##__VA_ARGS__))\
	      o 42) == 0;			\
}

B(50, __atomic_add_fetch, -, __ATOMIC_RELAXED)
B(51, __atomic_sub_fetch, +, __ATOMIC_RELAXED)
B(53, __atomic_xor_fetch, ^, __ATOMIC_RELAXED)
B(55, __sync_add_and_fetch, -)
B(56, __sync_sub_and_fetch, +)
B(58, __sync_xor_and_fetch, ^)
