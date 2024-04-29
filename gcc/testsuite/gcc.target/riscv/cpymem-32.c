/* { dg-do compile } */
/* { dg-require-effective-target rv32 } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -mtune=rocket" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-allow-blank-lines-in-output 1 } */

#define COPY_N(N)					\
void copy_##N (void *to, void *from)			\
{							\
  __builtin_memcpy (to, from, N);			\
}

#define COPY_ALIGNED_N(N)				\
void copy_aligned_##N (void *to, void *from)		\
{							\
  to = __builtin_assume_aligned(to, sizeof(long));	\
  from = __builtin_assume_aligned(from, sizeof(long));	\
  __builtin_memcpy (to, from, N);			\
}

/*
**copy_7:
**    ...
**    lbu\t[at][0-9],0\([at][0-9]\)
**    ...
**    sb\t[at][0-9],0\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],6\([at][0-9]\)
**    ...
**    sb\t[at][0-9],6\([at][0-9]\)
**    ...
*/
COPY_N(7)

/*
**copy_aligned_7:
**    ...
**    lw\t[at][0-9],0\([at][0-9]\)
**    sw\t[at][0-9],0\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],6\([at][0-9]\)
**    sb\t[at][0-9],6\([at][0-9]\)
**    ...
*/
COPY_ALIGNED_N(7)

/*
**copy_8:
**    ...
**    lbu\t[at][0-9],0\([at][0-9]\)
**    ...
**    sb\t[at][0-9],0\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],7\([at][0-9]\)
**    sb\t[at][0-9],7\([at][0-9]\)
**    ...
*/
COPY_N(8)

/*
**copy_aligned_8:
**    ...
**    lw\ta[0-9],0\(a[0-9]\)
**    sw\ta[0-9],0\(a[0-9]\)
**    ...
*/
COPY_ALIGNED_N(8)

/*
**copy_11:
**    ...
**    lbu\t[at][0-9],0\([at][0-9]\)
**    ...
**    sb\t[at][0-9],0\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],10\([at][0-9]\)
**    sb\t[at][0-9],10\([at][0-9]\)
**    ...
*/
COPY_N(11)

/*
**copy_aligned_11:
**    ...
**    lw\t[at][0-9],0\([at][0-9]\)
**    ...
**    sw\t[at][0-9],0\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],10\([at][0-9]\)
**    sb\t[at][0-9],10\([at][0-9]\)
**    ...
*/
COPY_ALIGNED_N(11)

/*
**copy_15:
**    ...
**    (call|tail)\tmemcpy
**    ...
*/
COPY_N(15)

/*
**copy_aligned_15:
**    ...
**    lw\t[at][0-9],0\([at][0-9]\)
**    ...
**    sw\t[at][0-9],0\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],14\([at][0-9]\)
**    sb\t[at][0-9],14\([at][0-9]\)
**    ...
*/
COPY_ALIGNED_N(15)

/*
**copy_27:
**    ...
**    (call|tail)\tmemcpy
**    ...
*/
COPY_N(27)

/*
**copy_aligned_27:
**    ...
**    lw\t[at][0-9],20\([at][0-9]\)
**    ...
**    sw\t[at][0-9],20\([at][0-9]\)
**    ...
**    lbu\t[at][0-9],26\([at][0-9]\)
**    sb\t[at][0-9],26\([at][0-9]\)
**    ...
*/
COPY_ALIGNED_N(27)
