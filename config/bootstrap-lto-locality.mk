# This option enables LTO and locality partitioning for stage2 and stage3 in slim mode

STAGE2_CFLAGS += -flto=jobserver -frandom-seed=1 -fipa-reorder-for-locality
STAGE3_CFLAGS += -flto=jobserver -frandom-seed=1 -fipa-reorder-for-locality
STAGEprofile_CFLAGS += -flto=jobserver -frandom-seed=1 -fipa-reorder-for-locality
STAGEtrain_CFLAGS += -flto=jobserver -frandom-seed=1 -fipa-reorder-for-locality
STAGEfeedback_CFLAGS += -flto=jobserver -frandom-seed=1 -fipa-reorder-for-locality

# assumes the host supports the linker plugin
LTO_AR = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-ar$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/
LTO_RANLIB = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-ranlib$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/
LTO_NM = $$r/$(HOST_SUBDIR)/prev-gcc/gcc-nm$(exeext) -B$$r/$(HOST_SUBDIR)/prev-gcc/

LTO_EXPORTS = AR="$(LTO_AR)"; export AR; \
	      RANLIB="$(LTO_RANLIB)"; export RANLIB; \
	      NM="$(LTO_NM)"; export NM;
LTO_FLAGS_TO_PASS = AR="$(LTO_AR)" RANLIB="$(LTO_RANLIB)" NM="$(LTO_NM)"

do-compare = $(SHELL) $(srcdir)/contrib/compare-lto $$f1 $$f2
extra-compare = gcc/lto1$(exeext)
