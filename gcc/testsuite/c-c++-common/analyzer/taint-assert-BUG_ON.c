/* We need this, otherwise the warnings are emitted inside the macros, which
   makes it hard to write the DejaGnu directives.  */
/* { dg-additional-options " -ftrack-macro-expansion=0" } */

/* Adapted from code in the Linux kernel, which has this: */
/* SPDX-License-Identifier: GPL-2.0 */

#define __noreturn __attribute__ ((__noreturn__))

void panic(const char *fmt, ...) __noreturn;

int _printk(const char *fmt, ...);
#define __printk_index_emit(...) do {} while (0)
#define printk_index_wrap(_p_func, _fmt, ...)				\
	({								\
		__printk_index_emit(_fmt, NULL, NULL);			\
		_p_func(_fmt, ##__VA_ARGS__);				\
	})
#define printk(fmt, ...) printk_index_wrap(_printk, fmt, ##__VA_ARGS__)
#define barrier_before_unreachable() do { } while (0)

#define BUG() do { \
	printk("BUG: failure at %s:%d/%s()!\n", __FILE__, __LINE__, __func__); \
	barrier_before_unreachable(); \
	panic("BUG!"); \
} while (0)

#define BUG_ON(condition) do { if (condition) BUG(); } while (0)

void __attribute__((tainted_args))
test_BUG(int n)
{
  if (n > 100) /* { dg-message "use of attacker-controlled value for control flow" } */
    BUG(); /* { dg-warning "-Wanalyzer-tainted-assertion" "warning" } */
  /* { dg-message "treating 'panic' as an assertion failure handler due to '__attribute__\\(\\(__noreturn__\\)\\)'" "final event" { target *-*-* } .-1 } */
}

void __attribute__((tainted_args))
test_BUG_ON(int n)
{
  BUG_ON(n > 100); /* { dg-warning "-Wanalyzer-tainted-assertion" "warning" } */
  /* { dg-message "treating 'panic' as an assertion failure handler due to '__attribute__\\(\\(__noreturn__\\)\\)'" "final event" { target *-*-* } .-1 } */
}

int __attribute__((tainted_args))
test_switch_BUG_1(int n)
{
  switch (n) { /* { dg-message "use of attacker-controlled value for control flow" } */
  default:
  case 0:
    return 5;
  case 1:
    return 22;
  case 2:
    return -1;
  case 42:
    BUG (); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
  }
}

int __attribute__((tainted_args))
test_switch_BUG(int n)
{
  switch (n) { /* { dg-message "use of attacker-controlled value for control flow" } */
  case 0:
    return 5;
  case 1:
    return 22;
  case 2:
    return -1;
  }
  BUG (); /* { dg-warning "-Wanalyzer-tainted-assertion" } */
}
