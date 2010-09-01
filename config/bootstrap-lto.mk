# This option enables LTO for stage2 and stage3.  It requires lto to
# be enabled for stage1 with --enable-stage1-languages.

STAGE2_CFLAGS += -fwhopr=jobserver -fuse-linker-plugin -frandom-seed=1
STAGE3_CFLAGS += -fwhopr=jobserver -fuse-linker-plugin -frandom-seed=1

# Ada fails to build with LTO, turn it off for now.
BOOT_ADAFLAGS += -fno-lto
