s/|| cp/|| copy/
s/rm -f/del/
/^Makefile/,/^	$(SHELL) config.run/d
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
/^version=/ c\
version=2.6.2
s/CC = cc/CC = cl/
s/^SHELL =.*/SHELL =/
s/CFLAGS = -g/CFLAGS =/
s/:\$/: \$/g
s/<\ *\$(srcdir)\//< $(srcdir)\\/g
s/^	\$(srcdir)\/move-if-change/	copy/
s/^USE_/# USE_/
s/`echo \$(srcdir)\///g
s/ | sed 's,\^\\\.\/,,'`//g
s/^	cd \$(srcdir)[ 	]*;/	/
/^stamp-attrtab/,/copy/ {
  /\\/d
  /fi/d
  /copy/ i\
	  genattrtab md > tmp-attrtab.c
}
/^enquire[ 	]*:/ s/\$(GCC_PARTS)//g
/^enquire.o[ 	]*:/ s/\$(GCC_PASSES)//g
/^GCC_FOR_TARGET =/ c\
GCC_FOR_TARGET = gcc
s/; *@true//
/^OBJS.*stamp-objlist/ s?`cat ../stamp-objlist`?@../stamp-objlist?
s/^\(SUBDIR_OBSTACK *=\).*$/\1 ..\/obstack.o/
s/^\(SUBDIR_USE_ALLOCA *=\).*$/\1/
s/^\(SUBDIR_MALLOC *=\).*$/\1/
/####target/ i\
STMP_FIXPROTO = \
OTHER_FIXINCLUDES_DIRS=/MSTOOLS/h \
RANLIB = : \
RANLIB_TEST = false \
OLDCC = cl \
MAKE = make \
SYMLINK = copy \
INSTALL = $(srcdir)/install.sh -c \
exeext = .exe \
objext = .obj \
oldobjext = .obj \
\
CC = cl \
CFLAGS = -Di386 -DWIN32 -D_WIN32 -DWINNT -D_M_IX86=300 -D_X86_=1 \\\
 -DALMOST_STDC -D_MSC_VER=800 \
CLIB =  libc.lib kernel32.lib \
LDFLAGS = -align:0x1000 -subsystem:console -entry:mainCRTStartup \\\
 -stack:1000000,1000000 \
\
EXTRA_PROGRAMS=ld.exe \
\
ld.obj: $(srcdir)/config/winnt/ld.c \
	$(CC) $(CFLAGS) \\\
 -I. -I$(srcdir) -I$(srcdir)/config -c $(srcdir)/config/winnt/ld.c \
\
ld.exe: ld.obj \
	link32 -out:ld.exe ld.obj $(LDFLAGS) $(CLIB)
s/^C c:/Cc:/
s/\${OBJS}/\$(OBJS)/g
s/\${SYSTEM_HEADER_DIR}/\$(SYSTEM_HEADER_DIR)/g
s/\${HOST_CC}/\$(HOST_CC)/g
s/ \${srcdir}\// /g
s/\${mainversion}/\$(mainversion)/g
s/\ $(srcdir)\/move-if-change$//
s/\$(srcdir)\/move-if-change/copy/g
/^# USE_HOST_OBSTACK/ i\
USE_HOST_OBSTACK=obstack.obj
/^# USE_ALLOCA/ i\
USE_ALLOCA=alloca.obj
/^# USE_HOST_ALLOCA/ i\
USE_HOST_ALLOCA=alloca.obj
s/^ALLOCA =/ALLOCA = alloca.obj/
s/^ALLOCA_FINISH = true/ALLOCA_FINISH =/
s/	\.\//	/
s/^bi-\([a-z]*\) *:/bi-\1.exe :/
s/ bi-\([a-z]*\)$/ bi-\1.exe/
s/ bi-\([a-z]*\) / bi-\1.exe /g
s/^gen\([a-z]*\) *:/gen\1.exe :/
s/ gen\([a-z]*\)$/ gen\1.exe/
s/ gen\([a-z]*\) / gen\1.exe /g
s/genmultilib.exe/genmultilib/g
s/^cccp *:/cccp.exe :/
s/cccp$/cccp.exe/
s/cccp /cccp.exe /
s/^cpp *:/cpp.exe :/
s/cpp$/cpp.exe/
s/cpp /cpp.exe /
s/^cc1 *:/cc1.exe :/
s/cc1$/cc1.exe/
s/cc1 /cc1.exe /
s/^cc1obj *:/cc1obj.exe :/
s/cc1obj$/cc1obj.exe/
s/cc1obj /cc1obj.exe /
s/^xgcc *:/xgcc.exe :/
s/xgcc$/xgcc.exe/
s/xgcc /xgcc.exe /
s/\.o *:/.obj :/
s/\.o$/.obj/
s/\.o /.obj /g
s/-rm -f cpp.exe/del cpp.exe/
s/\$(CC) \$(ALL_CFLAGS) \$(LDFLAGS) -o /link32 $(LDFLAGS) -out:/
s/\$(HOST_CC) \$(HOST_CFLAGS) \$(HOST_LDFLAGS) -o /link32 $(HOST_LDFLAGS) -out:/
s/^\//
