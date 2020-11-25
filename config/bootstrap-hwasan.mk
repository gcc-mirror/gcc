# This option enables -fsanitize=hwaddress for stage2 and stage3.

STAGE2_CFLAGS += -fsanitize=hwaddress
STAGE3_CFLAGS += -fsanitize=hwaddress
POSTSTAGE1_LDFLAGS += -fsanitize=hwaddress -static-libhwasan \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/hwasan/ \
		      -B$$r/prev-$(TARGET_SUBDIR)/libsanitizer/hwasan/.libs
