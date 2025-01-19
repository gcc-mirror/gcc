/* Test reduced from use of dynamic_pr_debug on Linux kernel, to verify that
   we treat the static struct _ddebug as not needing to be tracked by the
   analyzer, thus optimizing away bloat in the analyzer's state tracking.  */

/* { dg-do compile { target x86_64-*-* } } */
/* { dg-additional-options "-fdump-analyzer-untracked" } */

/* Adapted from various files in the Linux kernel, all of which have:  */
/* SPDX-License-Identifier: GPL-2.0 */

typedef struct {} atomic_t;

/* Adapted from include/linux/compiler_attributes.h  */
#define __always_inline                 inline __attribute__((__always_inline__))

/* Adapted from include/linux/compiler-gcc.h */
#define asm_volatile_goto(x...)	do { asm goto(x); asm (""); } while (0)

/* Adapted from include/linux/jump_label.h, which has:  */

struct static_key {};

/* Adapted from arch/x86/include/asm/jump_label.h */

static __always_inline bool arch_static_branch(struct static_key * const key, const bool branch)
{
	asm_volatile_goto("1:"
		: :  "i" (key), "i" (branch) : : l_yes);

	return false;
l_yes:
	return true;
}

static __always_inline bool arch_static_branch_jump(struct static_key * const key, const bool branch)
{
	asm_volatile_goto("1:"
		: :  "i" (key), "i" (branch) : : l_yes);

	return false;
l_yes:
	return true;
}

/* Adapted from include/linux/dynamic_debug.h  */

struct _ddebug {
	/* [...snip...] */
	const char *function;
	const char *filename;
	const char *format;
	unsigned int lineno:18;
	/* [...snip...] */
	unsigned int flags:8;
	struct static_key key;
} __attribute__((aligned(8)));

extern void __dynamic_pr_debug(struct _ddebug *descriptor, const char *fmt, ...);

static void expanded_dynamic_pr_debug(void) {
  do {
    static struct _ddebug __attribute__((__aligned__(8)))
    __attribute__((__section__("__dyndbg"))) __UNIQUE_ID_ddebug277 = { /* { dg-warning "track '__UNIQUE_ID_ddebug277': no" } */
        .function = __func__,
        .filename = __FILE__,
        .format = ("hello world"),
        .lineno = __LINE__,
        .flags = 0};
    if (arch_static_branch(&__UNIQUE_ID_ddebug277.key, false))
      __dynamic_pr_debug(&__UNIQUE_ID_ddebug277,
			 "hello world");
  } while (0);
}
