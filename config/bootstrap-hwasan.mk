# This option enables -fsanitize=hwaddress for stage2 and stage3.
# We need to disable random frame tags for bootstrap since the autoconf check
# for which direction the stack is growing has UB that a random frame tag
# breaks.  Running with a random frame tag gives approx. 50% chance of
# bootstrap comparison diff in libiberty/alloca.c.

STAGE2_CFLAGS += -fsanitize=hwaddress --param hwasan-random-frame-tag=0
STAGE3_CFLAGS += -fsanitize=hwaddress --param hwasan-random-frame-tag=0
POSTSTAGE1_LDFLAGS += -fsanitize=hwaddress -static-libhwasan \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/hwasan/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/hwasan/.libs
