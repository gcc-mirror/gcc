/^Makefile/,/^	rm -f config.run/d
s/rm -f/del/
s/|| cp/|| copy/
/^config.status/,/	fi/d
s/config.status//g
s/\/dev\/null/NUL/g
s/$(srcdir)\/c-parse/c-parse/g
s/$(srcdir)\/objc-parse.y/objc-parse.y/g
s/$(srcdir)\/c-gperf/c-gperf/g
/^multilib.h/ s/multilib/not-multilib/
/^xmake_file=/ d
/^tmake_file=/ d
/^lang_specs_files=/ d
/^lang_options_files=/ d
/^version=/ c\
version=2.8.1
s/CC = cc/CC = cl/
s/^SHELL =.*/SHELL =/
s/CFLAGS = -g/CFLAGS =/
s/:\$/: \$/g
s/<\ *\$(srcdir)\//< $(srcdir)\\/g
s/^	\$(SHELL) \$(srcdir)\/move-if-change/	copy/
s/^	\$(srcdir)\/move-if-change/	copy/
s/^USE_/# USE_/
s/`echo \$(srcdir)\///g
s/ | sed 's,\^\\\.\/,,'`//g
s/^	cd \$(srcdir)[ 	]*;/	/
/^stamp-attrtab/,/copy/ {
  /\\$/d
  /	fi/d
  /copy/ i\
\	  genattrtab $(md_file) > tmp-attrtab.c
}
/^enquire[ 	]*:/ s/\$(GCC_PARTS)//g
/^enquire.o[ 	]*:/ s/\$(GCC_PASSES)//g
/^GCC_FOR_TARGET =/ c\
GCC_FOR_TARGET = xgcc
/^ENQUIRE_LDFLAGS =/ c\
ENQUIRE_LDFLAGS =
s/; *@true//
/> *stamp-objlist/ c\
	echo.exe  $(OBJS) | sed -e "s, \([a-z]\), ../\1,g" >stamp-objlist
/^OBJS.*stamp-objlist/ s?`cat ../stamp-objlist`?@../stamp-objlist?
s/^\(SUBDIR_OBSTACK *=\).*$/\1 ..\/obstack.o/
s/^\(SUBDIR_USE_ALLOCA *=\).*$/\1/
s/^\(SUBDIR_MALLOC *=\).*$/\1/
/####target/ i\
STMP_FIXPROTO = \
OTHER_FIXINCLUDES_DIRS=. \
RANLIB = : \
RANLIB_TEST = false \
OLDCC = cl \
MAKE = nmake \
SYMLINK = copy \
INSTALL = $(srcdir)/install.sh -c \
exeext = .exe \
objext = .obj \
oldobjext = .obj \
\
EXTRA_PROGRAMS=ld.exe \
\
ld.obj: $(srcdir)/config/winnt/ld.c \
\	$(CC) $(CFLAGS) \\\
\ 	-I. -I$(srcdir) -I$(srcdir)/config -c $(srcdir)/config/winnt/ld.c \
\
ld.exe: ld.obj \
	link -out:ld.exe ld.obj $(LDFLAGS) $(CLIB) \
\
EXTRA_GCC_OBJS=spawnv.obj oldnames.obj \
spawnv.obj: $(srcdir)/config/winnt/spawnv.c \
\	$(CC) $(CFLAGS) \\\
\ 	-I. -I$(srcdir) -I$(srcdir)/config -c $(srcdir)/config/winnt/spawnv.c \
\
oldnames.obj: $(srcdir)/config/winnt/oldnames.c \
\	$(CC) $(CFLAGS) \\\
\ 	-I. -I$(srcdir) -I$(srcdir)/config -c $(srcdir)/config/winnt/oldnames.c
s/^C c:/Cc:/
s/\${OBJS}/\$(OBJS)/g
s/\${SYSTEM_HEADER_DIR}/\$(SYSTEM_HEADER_DIR)/g
s/\${HOST_CC}/\$(HOST_CC)/g
s/ \${srcdir}\// /g
s/\${mainversion}/\$(mainversion)/g
s/\$(srcdir)\/move-if-change/copy/g
s/\$(SHELL) \$(srcdir)\/move-if-change/copy/g
/^# USE_HOST_OBSTACK/ i\
USE_HOST_OBSTACK=obstack.obj
/^# USE_ALLOCA/ i\
USE_ALLOCA=alloca.obj
/^# USE_HOST_ALLOCA/ i\
USE_HOST_ALLOCA=alloca.obj
s/^ALLOCA =/ALLOCA = alloca.obj/
s/^ALLOCA_FINISH = true/ALLOCA_FINISH =/
s/	\.\//	/
s/^gen\([a-z]*\) *:/gen\1.exe :/
s/ gen\([a-z]*\)$/ gen\1.exe/
s/ gen\([a-z]*\) / gen\1.exe /g
s/genmultilib.exe/genmultilib/g
s/^cccp *:/cccp.exe :/
s/cccp$/cccp.exe/
s/cccp /cccp.exe /
s/CCCP=cccp.exe/CCCP=cccp/
s/(CCCP)$/(CCCP)$(exeext)/
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
s/^enquire *:/enquire.exe :/
s/enquire$/enquire.exe/
s/enquire /enquire.exe /
s/\.o *:/.obj :/
s/\.o$/.obj/
s/\.o /.obj /g
s/-rm -f cpp.exe/del cpp.exe/
s/\$(CC) \$(ALL_CFLAGS) \$(LDFLAGS) -o /link $(LDFLAGS) -out:/
s/\$(HOST_CC) \$(HOST_CFLAGS) \$(HOST_LDFLAGS) -o /link $(HOST_LDFLAGS) -out:/
/^# Build libgcc.a/ r config/winnt/libgcc.mak
/^# Build libgcc.a/,// d
/^# Build the include directory\./ r config/winnt/headers.mak
/^# Build the include directory\./,/touch objc-headers/ d
s/^\//
