# This option enables -fsanitize=address for stage1.

# Suppress LeakSanitizer in bootstrap.
export ASAN_OPTIONS=detect_leaks=0:use_odr_indicator=1

STAGE1_CFLAGS += -fsanitize=address
STAGE1_LDFLAGS += -fsanitize=address -static-libasan
