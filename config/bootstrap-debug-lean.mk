# This BUILD_CONFIG option is a bit like bootstrap-debug, but in
# addition to comparing stripped object files, it also compares
# compiler internal state during stage3.

# This makes it slower than bootstrap-debug, for there's additional
# dumping and recompilation during stage3.  bootstrap-debug-big can
# avoid the recompilation, if plenty of disk space is available.

STAGE2_CFLAGS += -gtoggle -fcompare-debug=
STAGE3_CFLAGS += -fcompare-debug
do-compare = $(SHELL) $(srcdir)/contrib/compare-debug $$f1 $$f2
