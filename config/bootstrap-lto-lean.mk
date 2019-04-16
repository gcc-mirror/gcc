# This option enables LTO for stage4 and LTO for generators in stage3 with profiledbootstrap.
# Otherwise, LTO is used in only stage3.

STAGE3_CFLAGS += -flto=jobserver
override STAGEtrain_CFLAGS := $(filter-out -flto=jobserver,$(STAGEtrain_CFLAGS))
STAGEtrain_GENERATOR_CFLAGS += -flto=jobserver
STAGEfeedback_CFLAGS += -flto=jobserver

# assumes the host supports the linker plugin
LTO_AR = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-ar$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/
LTO_RANLIB = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-ranlib$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/

LTO_EXPORTS = AR="$(LTO_AR)"; export AR; \
	      RANLIB="$(LTO_RANLIB)"; export RANLIB;
LTO_FLAGS_TO_PASS = AR="$(LTO_AR)" RANLIB="$(LTO_RANLIB)"

do-compare = /bin/true
