# This option enables LTO for stage2 and stage3 on
# hosts without linker plugin support.

STAGE2_CFLAGS += -flto=jobserver -frandom-seed=1 -ffat-lto-objects
STAGE3_CFLAGS += -flto=jobserver -frandom-seed=1 -ffat-lto-objects
STAGEprofile_CFLAGS += -flto=jobserver -frandom-seed=1
STAGEtrain_CFLAGS += -flto=jobserver -frandom-seed=1
STAGEfeedback_CFLAGS += -flto=jobserver -frandom-seed=1
do-compare = /bin/true
