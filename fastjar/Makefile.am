# Process this with automake to create Makefile.in

## We definitely don't want dependency tracking when using automake 1.4.
AUTOMAKE_OPTIONS = no-dependencies

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"JC1FLAGS=$(JC1FLAGS)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
	"SHELL=$(SHELL)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"prefix=$(prefix)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

INCLUDES = -I. -I$(top_srcdir) $(ZINCS) -I$(top_srcdir)/../include

LIBIBERTY = ../libiberty/libiberty.a

bin_PROGRAMS = jar grepjar
jar_SOURCES = jartool.c dostime.c compress.c pushback.c jartool.h \
zipfile.h dostime.h compress.h pushback.h
jar_LDADD = $(ZLIBS) $(LIBIBERTY)
jar_DEPENDENCIES = $(ZDEPS) $(LIBIBERTY)

grepjar_SOURCES = jargrep.c dostime.c compress.c pushback.c jartool.h \
zipfile.h dostime.h compress.h pushback.h
grepjar_LDADD = $(ZLIBS) $(LIBIBERTY)
grepjar_DEPENDENCIES = $(ZDEPS) $(LIBIBERTY)

AM_CFLAGS = @fastjar_warn_cflags@
AM_MAKEINFOFLAGS = -I $(srcdir)/../gcc/doc/include

TEXINFO_TEX = ../gcc/doc/include/texinfo.tex
info_TEXINFOS = fastjar.texi
fastjar_TEXINFOS = \
	../gcc/doc/include/gcc-common.texi \
	../gcc/doc/include/gpl.texi
man_MANS = jar.1 grepjar.1
EXTRA_DIST = $(man_MANS)

TEXI2POD = perl $(srcdir)/../contrib/texi2pod.pl
POD2MAN = pod2man --center="GNU" --release="gcc-@gcc_version@"

%.1: %.pod
	-($(POD2MAN) --section=1 $< > $(@).T$$$$ && \
		mv -f $(@).T$$$$ $@) || \
		(rm -f $(@).T$$$$ && exit 1)

.INTERMEDIATE: jar.pod grepjar.pod

jar.pod: $(srcdir)/fastjar.texi
	-$(TEXI2POD) -D jar $< > $@

grepjar.pod: $(srcdir)/fastjar.texi
	-$(TEXI2POD) -D grepjar $< > $@

# GCC LOCAL CHANGE
# The following commands allow us to release tarballs with the man pages
# and info documentation prebuilt.  This feature is enabled via 
# --enable-generated-files-in-srcdir in the configure script.

if GENINSRC
STAMP_GENINSRC = stamp-geninsrc
else
STAMP_GENINSRC =
endif

all-local: $(STAMP_GENINSRC) 

stamp-geninsrc: jar.1 grepjar.1 fastjar.info
	-cp -p $(top_builddir)/jar.1 $(srcdir)/jar.1
	-cp -p $(top_builddir)/grepjar.1 $(srcdir)/grepjar.1
	-cp -p $(top_builddir)/fastjar.info $(srcdir)/fastjar.info
	touch $@

CLEANFILES = stamp-geninsrc
MAINTAINERCLEANFILES = $(srcdir)/jar.1 \
                       $(srcdir)/grepjar.1 \
                       $(srcdir)/fastjar.info
