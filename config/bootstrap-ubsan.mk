# This option enables -fsanitize=undefined for stage2 and stage3.

STAGE2_CFLAGS += -fsanitize=undefined
STAGE3_CFLAGS += -fsanitize=undefined
POSTSTAGE1_LDFLAGS += -fsanitize=undefined -static-libubsan -lpthread -ldl \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ubsan/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ubsan/.libs
