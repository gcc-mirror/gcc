#/\.o[ 	]*:/ s/config.status//
/^multilib.h/ s/multilib/not-multilib/
/^target=/ c\
target=winnt3.1
/^xmake_file=/ d
/^tmake_file=/ d
/^version=/ c\
version=2.6.1
s/CC = cc/CC = cl/
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
EXTRA_PROGRAMS="ld-winnt.exe" \
\
ld.obj: $(srcdir)/config/winnt/ld.c \
	cl -D_SYSV -DWINNT -D_M_IX86_ -D_X86_ -D__STDC__=0 -DALMOST_STDC \\\
 -I. -I$(srcdir) -I$(srcdir)/config -c $(srcdir)/config/winnt/ld.c \
\
ld-winnt.exe: ld.obj \
	link32 -align:0x1000 -subsystem:console -entry:mainCRTStartup \\\
 -stack:1000000,1000000 -out:ld-winnt.exe ld.obj libc.lib kernel32.lib \
	copy ld-winnt.exe ld.exe
/####host/ r config/i386/x-winnt
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
s/ln cccp.exe cpp.exe/copy cccp.exe cpp.exe/
s/\$(CC) \$(ALL_CFLAGS) \$(LDFLAGS) -o /link32 $(LDFLAGS) -out:/
s/\$(HOST_CC) \$(HOST_CFLAGS) \$(HOST_LDFLAGS) -o /link32 $(HOST_LDFLAGS) -out:/
s/^\//
