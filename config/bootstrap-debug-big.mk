# This BUILD_CONFIG option is a bit like bootstrap-debug-lean, but it
# trades space for speed: instead of recompiling programs during
# stage3, it generates dumps during stage2 and stage3, saving them all
# until the final compare.

STAGE2_CFLAGS += -fdump-final-insns
STAGE3_CFLAGS += -fdump-final-insns
do-compare = $(SHELL) $(srcdir)/contrib/compare-debug $$f1 $$f2
