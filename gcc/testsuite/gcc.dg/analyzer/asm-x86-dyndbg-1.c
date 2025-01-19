/* Test reduced from use of dynamic_pr_debug on Linux kernel, to verify that
   we treat the static struct _ddebug as not needing to be tracked by the
   analyzer, thus optimizing away bloat in the analyzer's state tracking.  */

/* { dg-do compile { target x86_64-*-* } } */
/* { dg-additional-options "-fdump-analyzer-untracked" } */

/* Adapted from various files in the Linux kernel, all of which have:  */
/* SPDX-License-Identifier: GPL-2.0 */

typedef struct {
	int counter;
} atomic_t;

/* Adapted from include/linux/compiler_attributes.h  */
#define __always_inline                 inline __attribute__((__always_inline__))

/* Adapted from include/linux/compiler-gcc.h */
#define asm_volatile_goto(x...)	do { asm goto(x); asm (""); } while (0)

/* Adapted from include/linux/jump_label.h, which has:  */

struct static_key {
	atomic_t enabled;
	union {
		/* [...snip...] */
		struct jump_entry *entries;
		/* [...snip...] */
	};
};

struct static_key_true {
	struct static_key key;
};

struct static_key_false {
	struct static_key key;
};

extern bool ____wrong_branch_error(void);

/* Adapted from arch/x86/include/asm/jump_label.h */

#define JUMP_TABLE_ENTRY				\
	".pushsection __jump_table,  \"aw\" \n\t"	\
	/*_ASM_ALIGN*/ "\n\t"				\
	".long 1b - . \n\t"				\
	".long %l[l_yes] - . \n\t"			\
	/*_ASM_PTR*/ "%c0 + %c1 - .\n\t"		\
	".popsection \n\t"

static __always_inline bool arch_static_branch(struct static_key * const key, const bool branch)
{
	asm_volatile_goto("1:"
		/*".byte " __stringify(BYTES_NOP5) "\n\t" */
		JUMP_TABLE_ENTRY
		: :  "i" (key), "i" (branch) : : l_yes);

	return false;
l_yes:
	return true;
}

static __always_inline bool arch_static_branch_jump(struct static_key * const key, const bool branch)
{
	asm_volatile_goto("1:"
		"jmp %l[l_yes]\n\t"
		JUMP_TABLE_ENTRY
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
	union {
		struct static_key_true dd_key_true;
		struct static_key_false dd_key_false;
	} key;
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
    if (({
          bool branch;
          if (__builtin_types_compatible_p(
                  typeof(*&__UNIQUE_ID_ddebug277.key.dd_key_false),
                  struct static_key_true))
            branch = arch_static_branch_jump(
                &(&__UNIQUE_ID_ddebug277.key.dd_key_false)->key, false);
          else if (__builtin_types_compatible_p(
                       typeof(*&__UNIQUE_ID_ddebug277.key.dd_key_false),
                       struct static_key_false))
            branch = arch_static_branch(
                &(&__UNIQUE_ID_ddebug277.key.dd_key_false)->key, false);
          else
            branch = ____wrong_branch_error();
          __builtin_expect(!!(branch), 0);
        }))
      __dynamic_pr_debug(&__UNIQUE_ID_ddebug277,
			 "hello world");
  } while (0);
}
