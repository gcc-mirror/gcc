# This option enables LTO for stage2 and stage3.
# FIXME: Our build system is not yet able to use gcc-ar wrapper, so we need
# to go with -ffat-lto-objects. 

STAGE2_CFLAGS += -flto=jobserver -frandom-seed=1 -ffat-lto-objects
STAGE3_CFLAGS += -flto=jobserver -frandom-seed=1 -ffat-lto-objects
STAGEprofile_CFLAGS += -fno-lto
