/* { dg-do compile } */
/* { dg-options "-mips16 -mcode-readable=yes -fno-tree-vrp -fno-tree-dominator-opts" } */
/* { dg-skip-if "per-function expected output" { *-*-* } { "-flto" "-O0" "-Os" } { "" } } */

/* This testcase generates multiple constant pools within a function body.  */

#define C(a,b) \
  if (a > b)  goto gt; \
  if (a < b)  goto lt;

#define C4(x,b) C((x)[0], b) C((x)[1],b) C((x)[2],b) C((x)[3],b)
#define C16(x,y) C4(x, (y)[0]) C4(x, (y)[1]) C4(x, (y)[2]) C4(x, (y)[3])

#define C64(x,y) C16(x,y) C16(x+4,y) C16(x+8,y)
#define C256(x,y) C64(x,y) C64(x,y+4) C64(x,y+8)

unsigned foo(int x[64], int y[64])
{
  C256(x,y);

  return 0x01234567;
 gt:
  return 0x12345678;
 lt:
  return 0xF0123456;
}

/*  Check that:
1. The __pend symbol is emitted as STT_FUNCTION followed by instructions:
	.type	__pend_frob_<X>, @function	# Symbol # must match label.
__pend_foo_<X>: 				# The symbol must match.
	.insn
.L<Y>:

2. __pend symbol at end of function has type STT_OBJECT

	.type	__pend_foo_<X>, @object
__pend_foo_<X>:
	.insn
	.end	foo

  */

/* { dg-final { scan-assembler "\t\\.type\t(__pend_foo_\[0-9\]+), @function\n\\1:\n\t\\.insn\n.L\[0-9\]+:\n" } }  */
/* { dg-final { scan-assembler "\t\\.type\t(__pend_foo_\[0-9\]+), @object\n\\1:\n\t\\.end\tfoo\n" } }  */
