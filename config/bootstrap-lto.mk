# This option enables LTO for stage2 and stage3.

STAGE2_CFLAGS += -flto=jobserver -frandom-seed=1
STAGE3_CFLAGS += -flto=jobserver -frandom-seed=1
STAGEprofile_CFLAGS += -fno-lto
