# This option enables -fsanitize=address for stage2 and stage3.

STAGE2_CFLAGS += -fsanitize=address
STAGE3_CFLAGS += -fsanitize=address
POSTSTAGE1_LDFLAGS += -fsanitize=address -static-libasan \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/asan/.libs
