/\.o[ 	]*:/ s/config.status//
/^multilib.h/ s/multilib/not-multilib/
/^target=/ c\
target=go32
/^xmake_file=/ d
/^tmake_file=/ d
/^version=/ c\
version=2.6.1
s/CC = cc/CC = gcc/
s/:\$/: \$/g
s/^	\ *\.\//	go32 /
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
  /fi/d
  /update/ i\
	  go32 genattrtab md > t-attrtab.c
}
/^enquire[ 	]*:/ s/\$(GCC_PARTS)//g
/^enquire.o[ 	]*:/ s/\$(GCC_PASSES)//g
/^GCC_FOR_TARGET =/ c\
GCC_FOR_TARGET = gcc
s/; *@true//
s/stamp-/s-/g
s/tmp-/t-/g
/> *s-objlist/ c\
	echo.exe -o s-objlist $(addprefix ../,$(OBJS) $(BC_OBJS))
/^OBJS.*s-objlist/ s?`cat ../s-objlist`?@../s-objlist?
s/^\(SUBDIR_OBSTACK *=\).*$/\1 ..\/obstack.o/
s/^\(SUBDIR_USE_ALLOCA *=\).*$/\1/
s/^\(SUBDIR_MALLOC *=\).*$/\1/
