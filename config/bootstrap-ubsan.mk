# This option enables -fsanitize=undefined for stage2 and stage3.

STAGE2_CFLAGS += -fsanitize=undefined -DUBSAN_BOOTSTRAP
STAGE3_CFLAGS += -fsanitize=undefined -DUBSAN_BOOTSTRAP
POSTSTAGE1_LDFLAGS += -fsanitize=undefined -static-libubsan -DUBSAN_BOOTSTRAP \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ubsan/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ubsan/.libs
