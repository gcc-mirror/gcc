# This option enables -fcheck-pointer-bounds -mmpx for stage2 and stage3.

STAGE2_CFLAGS += -fcheck-pointer-bounds -mmpx -frandom-seed=1
STAGE3_CFLAGS += -fcheck-pointer-bounds -mmpx -frandom-seed=1
POSTSTAGE1_LDFLAGS += -fcheck-pointer-bounds -mmpx -frandom-seed=1 \
		      -static-libmpx -static-libmpxwrappers \
		      -B$$r/prev-$(TARGET_SUBDIR)/libmpx \
		      -B$$r/prev-$(TARGET_SUBDIR)/libmpx/mpxrt/.libs \
		      -B$$r/prev-$(TARGET_SUBDIR)/libmpx/mpxwrap/.libs
