/\.o[ 	]*:/ s/config.status//
/^multilib.h/ s/multilib/not-multilib/
/^target=/ c\
target=go32
/^out_file=/ c\
out_file=config/i386/i386.c
/^out_object_file=/ c\
out_object_file=i386.o
/^md_file=/ c\
md_file=config/i386/i386.md
/^tm_file=/ c\
tm_file=config/i386/go32.h
/^build_xm_file=/ c\
build_xm_file=config/i386/xm-dos.h
/^host_xm_file=/ c\
host_xm_file=config/i386/xm-dos.h
/^lang_specs_files=/ d
/^lang_options_files=/ d
/^xmake_file=/ d
/^tmake_file=/ d
/^version=/ c\
version=2.8.1
/^mainversion=/ c\
mainversion=2.8.1
s/CC = cc/CC = gcc/
s/:\$/: \$/g
s/^	\ *\.\//	/
s/<\ *\$(srcdir)\//< $(srcdir)\\/g
s/^	\$(srcdir)\/move-if-change/	update/
s/^USE_/# USE_/
s/`echo \$(srcdir)\///g
s/ | sed 's,\^\\\.\/,,'`//g
s/^	cd \$(srcdir)[ 	]*;/	/
/^# USE_HOST_OBSTACK/ i\
USE_HOST_OBSTACK=obstack.o
/^stamp-attrtab/,/update/ {
  /\\/d
  /[ 	]fi[ 	]/d
  /[ 	]fi$/d
  /update/ i\
	genattrtab md > t-attrtab.c
}
/^enquire[ 	]*:/ s/\$(GCC_PARTS)//g
/^enquire.o[ 	]*:/ s/\$(GCC_PASSES)//g
/^GCC_FOR_TARGET =/ c\
GCC_FOR_TARGET = gcc
s/; *@true//
s/stamp-/s-/g
s/tmp-/t-/g
/> *s-objlist/ c\
	echo.exe -o s-objlist $(addprefix ../,$(OBJS))
/^OBJS.*s-objlist/ s?`cat ../s-objlist`?@../s-objlist?
s/^\(SUBDIR_OBSTACK *=\).*$/\1 ..\/obstack.o/
s/^\(SUBDIR_USE_ALLOCA *=\).*$/\1/
s/^\(SUBDIR_MALLOC *=\).*$/\1/
/^# Build libgcc.a/ r config/msdos/libgcc.mak
/^# Build libgcc.a/,// d
