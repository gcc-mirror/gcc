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

TEXINFO_TEX = ../gcc/doc/include/texinfo.tex
info_TEXINFOS = fastjar.texi
fastjar_TEXINFOS = \
	../gcc/doc/include/gcc-common.texi \
	../gcc/doc/include/fdl.texi \
	../gcc/doc/include/gpl.texi
man_MANS = jar.1 grepjar.1
EXTRA_DIST = $(man_MANS)

## This is a hack.  We can't set AM_MAKEINFOFLAGS, since that isn't
## available in 1.4.  Nor can we override or append to MAKEINFO or
## MAKEINFOFLAGS, since these are overridden by the top-level
## Makefile.  So, we just duplicate the rules.  FIXME: remove this
## when we upgrade automake.  Note that we don't include $(srcdir) in
## my_makei_flags; makeinfo is run in srcdir.
my_makei_flags += -I ../gcc/doc/include
fastjar.info: fastjar.texi $(fastjar_TEXINFOS)
	@cd $(srcdir) && rm -f $@ $@-[0-9] $@-[0-9][0-9]
	cd $(srcdir) \
	  && $(MAKEINFO) $(my_makei_flags) `echo $< | sed 's,.*/,,'`


TEXI2POD = perl $(srcdir)/../contrib/texi2pod.pl
POD2MAN = pod2man --center="GNU" --release="gcc-@gcc_version@"

$(srcdir)/jar.1: $(srcdir)/fastjar.texi
	-$(TEXI2POD) -D jar < $(srcdir)/fastjar.texi > fastjar.pod
	($(POD2MAN) --section=1 fastjar.pod > jar.1.T$$$$ && \
		mv -f jar.1.T$$$$ $(srcdir)/jar.1) || \
		(rm -f jar.1.T$$$$ && exit 1)
	rm -f fastjar.pod

$(srcdir)/grepjar.1: $(srcdir)/fastjar.texi
	-$(TEXI2POD) -D grepjar < $(srcdir)/fastjar.texi > grepjar.pod
	($(POD2MAN) --section=1 grepjar.pod > grepjar.1.T$$$$ && \
		mv -f grepjar.1.T$$$$ $(srcdir)/grepjar.1) || \
		(rm -f grepjar.1.T$$$$ && exit 1)
	rm -f grepjar.pod
