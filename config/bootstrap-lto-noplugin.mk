# This option enables LTO for stage2 and stage3 on
# hosts without linker plugin support.

STAGE2_CFLAGS += -flto=jobserver -frandom-seed=1 -ffat-lto-objects
STAGE3_CFLAGS += -flto=jobserver -frandom-seed=1 -ffat-lto-objects
STAGEprofile_CFLAGS += -fno-lto
