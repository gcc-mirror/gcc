# This BUILD_CONFIG option is a bit like bootstrap-debug, but rather
# than comparing stripped object files, it compares compiler internal
# state during stage3.  Both can be used simultaneously.

# This makes it slower than bootstrap-debug alone, for there's
# additional dumping and recompilation during stage3.
# bootstrap-debug-big can avoid the recompilation, if plenty of disk
# space is available.

STAGE2_CFLAGS += -fcompare-debug=
STAGE3_CFLAGS += -fcompare-debug
