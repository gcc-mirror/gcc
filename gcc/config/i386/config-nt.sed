#/\.o[ 	]*:/ s/config.status//
/^multilib.h/ s/multilib/not-multilib/
/^target=/ c\
target=winnt3.1
/^xmake_file=/ d
/^tmake_file=/ d
/^version=/ c\
version=2.6.0
s/CC = cc/CC = gcc/
s/:\$/: \$/g
s/<\ *\$(srcdir)\//< $(srcdir)\\/g
s/^	\$(srcdir)\/move-if-change/	copy/
s/^USE_/# USE_/
s/`echo \$(srcdir)\///g
s/ | sed 's,\^\\\.\/,,'`//g
s/^	cd \$(srcdir)[ 	]*;/	/
/^# USE_HOST_OBSTACK/ i\
USE_HOST_OBSTACK=obstack.o
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
/####target/ r config/winnt/t-winnt
/####host/ r config/winnt/x-winnt
s/^C c:/# C c:/
s/\${OBJS}/\$(OBJS)/g
s/\${SYSTEM_HEADER_DIR}/\$(SYSTEM_HEADER_DIR)/g
s/\${HOST_CC}/\$(HOST_CC)/g
s/ \${srcdir}\// /g
s/\${mainversion}/\$(mainversion)/g
s/\ $(srcdir)\/move-if-change$//
s/\$(srcdir)\/move-if-change/mv/g
/^# USE_HOST_OBSTACK/ i\
USE_HOST_OBSTACK=obstack.o
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
s/^cc1 *:/cc1.exe :/
s/cc1$/cc1.exe/
s/cc1 /cc1.exe /
s/^xgcc *:/xgcc.exe :/
s/xgcc$/xgcc.exe/
s/xgcc /xgcc.exe /
s/^ld *:/ld.exe :/
s/ld$/ld.exe/
s/ld /ld.exe /
s/^\//
