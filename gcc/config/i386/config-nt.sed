/^Makefile/,/^	rm -f config.run/d
s/rm -f/del/
s/|| cp/|| copy/
/^config.status/,/	fi/d
s/config.status//g
s/\/dev\/null/NUL/g
s/$(srcdir)\/c-parse/c-parse/g
s/$(srcdir)\/c-gperf/c-gperf/g
/^multilib.h/ s/multilib/not-multilib/
/^target=/ c\
target=winnt3.5
/^xmake_file=/ d
/^tmake_file=/ d
/^out_file/ c\
out_file=config/i386/i386.c
/^out_object_file/ c\
out_object_file=i386.obj
/^md_file/ c\
md_file=config/i386/i386.md
/^tm_file/ c\
tm_file=config/i386/win-nt.h
/^build_xm_file/ c\
build_xm_file=config/i386/xm-winnt.h
/^host_xm_file/ c\
host_xm_file=config/i386/xm-winnt.h
/^####target/ i\
CC = cl \
CLIB = libc.lib kernel32.lib \
CFLAGS = -Di386 -DWIN32 -D_WIN32 -D_M_IX86=300 -D_X86_=1 \\\
 -DALMOST_STDC -D_MSC_VER=800 \
LDFLAGS = -align:0x1000 -subsystem:console -entry:mainCRTStartup \\\
 -stack:1000000,1000 \
\
EXTRA_OBJS=winnt.obj \
winnt.obj: $(srcdir)/config/i386/winnt.c \
\	$(CC) $(CFLAGS) \\\
\ 	-I. -I$(srcdir) -I$(srcdir)/config -c $(srcdir)/config/i386/winnt.c \

