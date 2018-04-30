# This option enables LTO for stage2 and stage3 in slim mode

STAGE2_CFLAGS += -flto=jobserver -frandom-seed=1
STAGE3_CFLAGS += -flto=jobserver -frandom-seed=1
STAGEprofile_CFLAGS += -flto=jobserver -frandom-seed=1
STAGEtrain_CFLAGS += -flto=jobserver -frandom-seed=1
STAGEfeedback_CFLAGS += -flto=jobserver -frandom-seed=1

# assumes the host supports the linker plugin
LTO_AR = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-ar$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/
LTO_RANLIB = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-ranlib$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/

LTO_EXPORTS = AR="$(LTO_AR)"; export AR; \
	      RANLIB="$(LTO_RANLIB)"; export RANLIB;
LTO_FLAGS_TO_PASS = AR="$(LTO_AR)" RANLIB="$(LTO_RANLIB)"
