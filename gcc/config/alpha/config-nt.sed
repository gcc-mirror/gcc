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
out_file=config/alpha/alpha.c
/^out_object_file/ c\
out_object_file=alpha.obj
/^md_file/ c\
md_file=config/alpha/alpha.md
/^tm_file/ c\
tm_file=config/alpha/winnt.h
/^build_xm_file/ c\
build_xm_file=config/alpha/xm-winnt.h
/^host_xm_file/ c\
host_xm_file=config/alpha/xm-winnt.h
/^####target/ i\
CC = cl \
CLIB = libc.lib kernel32.lib \
CFLAGS = -Dalpha -DWIN32 -D_WIN32 -D_ALPHA_ -D_M_ALPHA \\\
 -DALMOST_STDC \
LDFLAGS = -subsystem:console -entry:mainCRTStartup \\\
 -stack:1000000,1000 \

