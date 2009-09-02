# This BUILD_CONFIG option tests that target libraries built during
# stage3 would have generated the same executable code if they were
# compiled with -g0.

# It uses -g0 rather than -gtoggle because -g is default on target
# library builds, and toggling it where it's supposed to be disabled
# breaks e.g. crtstuff on ppc.

STAGE1_TFLAGS += -g0 -fcompare-debug=
STAGE2_TFLAGS += -fcompare-debug=
STAGE3_TFLAGS += -fcompare-debug=-g0
do-compare = $(SHELL) $(srcdir)/contrib/compare-debug $$f1 $$f2
