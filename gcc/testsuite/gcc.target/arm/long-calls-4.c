/* Check that long calls to different sections are not optimized to "bl".  */
/* { dg-do compile { target { arm32 && fpic } } } */
/* { dg-options "-O2 -fpic -mlong-calls" } */

#define section(S) __attribute__((section(S)))
#define weak __attribute__((weak))
#define noinline __attribute__((noinline))
#define long_call __attribute__((long_call))
#define short_call __attribute__((short_call))

#define REMOTE_CALL(ID, TARGET_ATTRS, CALL_ATTRS)			\
  const char *TARGET_ATTRS ID (void);					\
  const char *CALL_ATTRS call_##ID (void) { return ID () + 1; }

#define EXTERN_CALL(ID, TARGET_ATTRS, CALL_ATTRS)			\
  const char *TARGET_ATTRS noinline ID (void) { return #ID; }		\
  const char *CALL_ATTRS call_##ID (void) { return ID () + 1; }		\
  const char *CALL_ATTRS sibcall_##ID (void) { return ID (); }

#define STATIC_CALL(ID, TARGET_ATTRS, CALL_ATTRS)			\
  static const char *TARGET_ATTRS noinline ID (void) { return #ID; }	\
  const char *CALL_ATTRS call_##ID (void) { return ID () + 1; }		\
  const char *CALL_ATTRS sibcall_##ID (void) { return ID (); }

#define DO_TESTS_SECTION(ID, TEST, TARGET_ATTRS)			\
  TEST (ID##1, TARGET_ATTRS, )						\
  TEST (ID##2, TARGET_ATTRS section (".test.a"), section (".test.b"))	\
  TEST (ID##3, TARGET_ATTRS section (".test.c"), section (".test.c"))

#define DO_TESTS_CALL_ATTR(ID, TEST, TARGET_ATTRS)			\
  DO_TESTS_SECTION (ID##n, TEST, TARGET_ATTRS)				\
  DO_TESTS_SECTION (ID##l, TEST, TARGET_ATTRS long_call)		\
  DO_TESTS_SECTION (ID##s, TEST, TARGET_ATTRS short_call)

DO_TESTS_CALL_ATTR (remote_, REMOTE_CALL,)
DO_TESTS_CALL_ATTR (strong_, EXTERN_CALL,)
DO_TESTS_CALL_ATTR (weak_, EXTERN_CALL, weak)
DO_TESTS_CALL_ATTR (static_, STATIC_CALL,)


/* Calls to remote_*, strong_* and weak_* should honor the call type
   attribute, with "long" being the default.  

   In the regular expressions below:
   
   * The PLT marker is optional, even though we are using -fpic,
     because it is not used (or required) on some targets.

   * We allow both "b" and "bl" in some cases to allow for the
     possibility of sibling calls.  As of this writing, GCC does not
     use sibling calls in Thumb-2 mode.  */

/* { dg-final { scan-assembler-not "\tbl\tremote_n1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl\tremote_n2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl\tremote_n3(\\(PLT\\))?\n" } } */

/* { dg-final { scan-assembler-not "\tbl\tremote_l1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl\tremote_l2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl\tremote_l3(\\(PLT\\))?\n" } } */

/* { dg-final { scan-assembler "\tbl\tremote_s1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl\tremote_s2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl\tremote_s3(\\(PLT\\))?\n" } } */


/* { dg-final { scan-assembler-not "\tbl?\tstrong_n1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tstrong_n2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tstrong_n3(\\(PLT\\))?\n" } } */

/* { dg-final { scan-assembler-not "\tbl?\tstrong_l1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tstrong_l2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tstrong_l3(\\(PLT\\))?\n" } } */

/* { dg-final { scan-assembler "\tbl\tstrong_s1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstrong_s1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl\tstrong_s2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstrong_s2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl\tstrong_s3(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstrong_s3(\\(PLT\\))?\n" } } */


/* { dg-final { scan-assembler-not "\tbl?\tweak_n1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tweak_n2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tweak_n3(\\(PLT\\))?\n" } } */

/* { dg-final { scan-assembler-not "\tbl?\tweak_l1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tweak_l2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tweak_l3(\\(PLT\\))?\n" } } */

/* { dg-final { scan-assembler "\tbl\tweak_s1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl?\tweak_s1(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl\tweak_s2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl?\tweak_s2(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl\tweak_s3(\\(PLT\\))?\n" } } */
/* { dg-final { scan-assembler "\tbl?\tweak_s3(\\(PLT\\))?\n" } } */


/* Calls to static_*2 calls should honor the call type attribute,
   with "long" being the default.  Calls to other static_* functions
   should be short.  */

/* { dg-final { scan-assembler "\tbl\tstatic_n1((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_n1((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tstatic_n2((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl\tstatic_n3((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_n3((\\(PLT\\))?)\n" } } */

/* { dg-final { scan-assembler "\tbl\tstatic_l1((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_l1((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler-not "\tbl?\tstatic_l2((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl\tstatic_l3((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_l3((\\(PLT\\))?)\n" } } */

/* { dg-final { scan-assembler "\tbl\tstatic_s1((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_s1((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl\tstatic_s2((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_s2((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl\tstatic_s3((\\(PLT\\))?)\n" } } */
/* { dg-final { scan-assembler "\tbl?\tstatic_s3((\\(PLT\\))?)\n" } } */
