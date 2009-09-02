# This BUILD_CONFIG option is to be used along with
# bootstrap-debug-lean and bootstrap-debug-lib in a full bootstrap, to
# check that all host and target files are built with -fcompare-debug.

# These arrange for a simple warning to be issued if -fcompare-debug
# is not given.
# BOOT_CFLAGS += -fcompare-debug="-w%n-fcompare-debug not overridden"
# TFLAGS += -fcompare-debug="-w%n-fcompare-debug not overridden"

# GCC_COMPARE_DEBUG="-w%n-fcompare-debug not overridden";

FORCE_COMPARE_DEBUG = \
	GCC_COMPARE_DEBUG=$${GCC_COMPARE_DEBUG--fcompare-debug-not-overridden}; \
	export GCC_COMPARE_DEBUG;
POSTSTAGE1_HOST_EXPORTS += $(FORCE_COMPARE_DEBUG)
BASE_TARGET_EXPORTS += $(FORCE_COMPARE_DEBUG)
