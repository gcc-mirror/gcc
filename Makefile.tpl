[+ AutoGen5 template -*- Mode: Makefile -*-
in
+]

# Makefile.in is generated from Makefile.tpl by 'autogen Makefile.def'.
#
# Makefile for directory with subdirs to build.
#   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
#   1999, 2000, 2001, 2002, 2003 Free Software Foundation
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
#

# -------------------------------
# Standard Autoconf-set variables
# -------------------------------
VPATH=@srcdir@

build_alias=@build_alias@
build=@build@
host_alias=@host_alias@
host=@host@
target_alias=@target_alias@
target=@target@

program_transform_name = @program_transform_name@

prefix = @prefix@
exec_prefix = @exec_prefix@

srcdir = @srcdir@

bindir = @bindir@
sbindir = @sbindir@
libexecdir = @libexecdir@
datadir = @datadir@
sysconfdir = @sysconfdir@
sharedstatedir = @sharedstatedir@
localstatedir = @localstatedir@
libdir = @libdir@
includedir = @includedir@
oldincludedir = @oldincludedir@
infodir = @infodir@
mandir = @mandir@
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9

INSTALL = @INSTALL@
INSTALL_PROGRAM = @INSTALL_PROGRAM@
INSTALL_SCRIPT = @INSTALL_SCRIPT@
INSTALL_DATA = @INSTALL_DATA@

# -------------------------------------------------
# Miscellaneous non-standard autoconf-set variables
# -------------------------------------------------

links=@configlinks@
# The file containing GCC's version number.
gcc_version_trigger = @gcc_version_trigger@
gcc_version = @gcc_version@

# The gcc driver likes to know the arguments it was configured with.
TOPLEVEL_CONFIGURE_ARGUMENTS=@TOPLEVEL_CONFIGURE_ARGUMENTS@

gxx_include_dir = @gxx_include_dir@
libstdcxx_incdir = @libstdcxx_incdir@

tooldir = @tooldir@
build_tooldir = @build_tooldir@

# Directory in which the compiler finds executables, libraries, etc.
libsubdir = $(libdir)/gcc/$(target_alias)/$(gcc_version)
GDB_NLM_DEPS = 

# This is the name of the environment variable used for the path to
# the libraries.
RPATH_ENVVAR = @RPATH_ENVVAR@

# This is set by configure to REALLY_SET_LIB_PATH if --enable-shared
# was used.
SET_LIB_PATH = @SET_LIB_PATH@

# configure.in sets SET_LIB_PATH to this if --enable-shared was used.
# Some platforms don't like blank entries, so we remove duplicate,
# leading and trailing colons.
REALLY_SET_LIB_PATH = \
  @SET_GCC_LIB_PATH@ \
  $(RPATH_ENVVAR)=`echo "$(HOST_LIB_PATH):$(TARGET_LIB_PATH):$$$(RPATH_ENVVAR)" | sed 's,::*,:,g;s,^:*,,;s,:*$$,,'`; export $(RPATH_ENVVAR);

# This is the list of directories to be built for the build system.
BUILD_CONFIGDIRS = libiberty
# Build programs are put under this directory.
BUILD_SUBDIR = @build_subdir@
# This is set by the configure script to the arguments to use when configuring
# directories built for the build system.
BUILD_CONFIGARGS = @build_configargs@

# This is the list of directories to built for the host system.
SUBDIRS = @configdirs@
# This is set by the configure script to the arguments to use when configuring
# directories built for the host system.
HOST_CONFIGARGS = @host_configargs@

# This is set by the configure script to the list of directories which
# should be built using the target tools.
TARGET_CONFIGDIRS = @target_configdirs@
# Target libraries are put under this directory:
TARGET_SUBDIR = @target_subdir@
# This is set by the configure script to the arguments to use when configuring
# directories built for the target.
TARGET_CONFIGARGS = @target_configargs@

# ----------------------------------------------
# Programs producing files for the BUILD machine
# ----------------------------------------------

SHELL = @config_shell@

# pwd command to use.  Allow user to override default by setting PWDCMD in
# the environment to account for automounters.  The make variable must not
# be called PWDCMD, otherwise the value set here is passed to make
# subprocesses and overrides the setting from the user's environment.
# Don't use PWD since it is a common shell environment variable and we
# don't want to corrupt it.
PWD_COMMAND = $${PWDCMD-pwd}

# compilers to use to create programs which must be run in the build
# environment.
CC_FOR_BUILD = @CC_FOR_BUILD@
CFLAGS_FOR_BUILD = @CFLAGS_FOR_BUILD@

CXX_FOR_BUILD = $(CXX)

# Special variables passed down in EXTRA_GCC_FLAGS.  They are defined
# here so that they can be overridden by Makefile fragments.
BUILD_PREFIX = @BUILD_PREFIX@
BUILD_PREFIX_1 = @BUILD_PREFIX_1@

BISON=@BISON@
USUAL_BISON = `if [ -f $$r/bison/bison ] ; then \
	    echo $$r/bison/bison -L $$s/bison/ ; \
	 else \
	    echo bison ; \
	 fi`

DEFAULT_YACC = @DEFAULT_YACC@
YACC=@YACC@
USUAL_YACC = `if [ -f $$r/bison/bison ] ; then \
	    echo $$r/bison/bison -y -L $$s/bison/ ; \
	elif [ -f $$r/byacc/byacc ] ; then \
	    echo $$r/byacc/byacc ; \
	else \
	    echo ${DEFAULT_YACC} ; \
	fi`

DEFAULT_LEX = @DEFAULT_LEX@
LEX=@LEX@
USUAL_LEX = `if [ -f $$r/flex/flex ] ; \
	then echo $$r/flex/flex ; \
	else echo ${DEFAULT_LEX} ; fi`

DEFAULT_M4 = @DEFAULT_M4@
M4 = `if [ -f $$r/m4/m4 ] ; \
	then echo $$r/m4/m4 ; \
	else echo ${DEFAULT_M4} ; fi`

# For an installed makeinfo, we require it to be from texinfo 4.2 or
# higher, else we use the "missing" dummy.
MAKEINFO=@MAKEINFO@
USUAL_MAKEINFO = `if [ -f $$r/texinfo/makeinfo/makeinfo ] ; \
	then echo $$r/texinfo/makeinfo/makeinfo ; \
	else if (makeinfo --version \
	  | egrep 'texinfo[^0-9]*([1-3][0-9]|4\.[2-9]|[5-9])') >/dev/null 2>&1; \
        then echo makeinfo; else echo $$s/missing makeinfo; fi; fi`

# This just becomes part of the MAKEINFO definition passed down to
# sub-makes.  It lets flags be given on the command line while still
# using the makeinfo from the object tree.
# (Default to avoid splitting info files by setting the threshold high.)
MAKEINFOFLAGS = --split-size=5000000

EXPECT = `if [ -f $$r/expect/expect ] ; \
	then echo $$r/expect/expect ; \
	else echo expect ; fi`

RUNTEST = `if [ -f $$s/dejagnu/runtest ] ; \
	then echo $$s/dejagnu/runtest ; \
	else echo runtest ; fi`

# ---------------------------------------------
# Programs producing files for the HOST machine
# ---------------------------------------------

# This is the list of directories that may be needed in RPATH_ENVVAR
# so that programs built for the host machine work.
HOST_LIB_PATH = $$r/bfd:$$r/opcodes

AS = @AS@

AR = @AR@
AR_FLAGS = rc

CC = @CC@
CFLAGS = @CFLAGS@
LIBCFLAGS = $(CFLAGS)

CXX = @CXX@
CXXFLAGS = @CXXFLAGS@
LIBCXXFLAGS = $(CXXFLAGS) -fno-implicit-templates

DLLTOOL = @DLLTOOL@

NM = @NM@

LD = @LD@
LDFLAGS = 

RANLIB = @RANLIB@

WINDRES = @WINDRES@

PICFLAG = 

# -----------------------------------------------
# Programs producing files for the TARGET machine
# -----------------------------------------------

# This is the list of directories that may be needed in RPATH_ENVVAR
# so that prorgams built for the target machine work.
TARGET_LIB_PATH = $$r/$(TARGET_SUBDIR)/libstdc++-v3/src/.libs:

FLAGS_FOR_TARGET = @FLAGS_FOR_TARGET@

AR_FOR_TARGET=@AR_FOR_TARGET@
USUAL_AR_FOR_TARGET = ` \
  if [ -f $$r/binutils/ar ] ; then \
    echo $$r/binutils/ar ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(AR); \
    else \
       echo ar | sed '$(program_transform_name)' ; \
    fi; \
  fi`

AS_FOR_TARGET=@AS_FOR_TARGET@
USUAL_AS_FOR_TARGET = ` \
  if [ -f $$r/gas/as-new ] ; then \
    echo $$r/gas/as-new ; \
  elif [ -f $$r/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=as ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(AS); \
    else \
       echo as | sed '$(program_transform_name)' ; \
    fi; \
  fi`

CC_FOR_TARGET = @CC_FOR_TARGET@
# During gcc bootstrap, if we use some random cc for stage1 then
# CFLAGS will be just -g.  We want to ensure that TARGET libraries
# (which we know are built with gcc) are built with optimizations so
# prepend -O2 when setting CFLAGS_FOR_TARGET.
CFLAGS_FOR_TARGET = -O2 $(CFLAGS)
# If GCC_FOR_TARGET is not overriden on the command line, then this
# variable is passed down to the gcc Makefile, where it is used to
# build libgcc2.a.  We define it here so that it can itself be
# overridden on the command line.
GCC_FOR_TARGET=@GCC_FOR_TARGET@
USUAL_GCC_FOR_TARGET = $(STAGE_CC_WRAPPER) $$r/gcc/xgcc -B$$r/gcc/ $(FLAGS_FOR_TARGET)
LIBCFLAGS_FOR_TARGET = $(CFLAGS_FOR_TARGET)

CXX_FOR_TARGET = @CXX_FOR_TARGET@
RAW_CXX_FOR_TARGET = @RAW_CXX_FOR_TARGET@
CXX_FOR_TARGET_FOR_RECURSIVE_MAKE = @CXX_FOR_TARGET_FOR_RECURSIVE_MAKE@
RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE = @RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE@
CXXFLAGS_FOR_TARGET = $(CXXFLAGS)
LIBCXXFLAGS_FOR_TARGET = $(CXXFLAGS_FOR_TARGET) -fno-implicit-templates

DLLTOOL_FOR_TARGET=@DLLTOOL_FOR_TARGET@
USUAL_DLLTOOL_FOR_TARGET = ` \
  if [ -f $$r/binutils/dlltool ] ; then \
    echo $$r/binutils/dlltool ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(DLLTOOL); \
    else \
       echo dlltool | sed '$(program_transform_name)' ; \
    fi; \
  fi`

GCJ_FOR_TARGET = @GCJ_FOR_TARGET@

LD_FOR_TARGET=@LD_FOR_TARGET@
USUAL_LD_FOR_TARGET = ` \
  if [ -f $$r/ld/ld-new ] ; then \
    echo $$r/ld/ld-new ; \
  elif [ -f $$r/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=ld ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(LD); \
    else \
       echo ld | sed '$(program_transform_name)' ; \
    fi; \
  fi`

LDFLAGS_FOR_TARGET = 

NM_FOR_TARGET=@NM_FOR_TARGET@
USUAL_NM_FOR_TARGET = ` \
  if [ -f $$r/binutils/nm-new ] ; then \
    echo $$r/binutils/nm-new ; \
  elif [ -f $$r/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=nm ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(NM); \
    else \
       echo nm | sed '$(program_transform_name)' ; \
    fi; \
  fi`

RANLIB_FOR_TARGET=@RANLIB_FOR_TARGET@
USUAL_RANLIB_FOR_TARGET = ` \
  if [ -f $$r/binutils/ranlib ] ; then \
    echo $$r/binutils/ranlib ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      if [ x'$(RANLIB)' != x ]; then \
         echo $(RANLIB); \
      else \
         echo ranlib; \
      fi; \
    else \
       echo ranlib | sed '$(program_transform_name)' ; \
    fi; \
  fi`

WINDRES_FOR_TARGET=@WINDRES_FOR_TARGET@
USUAL_WINDRES_FOR_TARGET = ` \
  if [ -f $$r/binutils/windres ] ; then \
    echo $$r/binutils/windres ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(WINDRES); \
    else \
       echo windres | sed '$(program_transform_name)' ; \
    fi; \
  fi`

PICFLAG_FOR_TARGET = 

# ------------------------------------
# Miscellaneous targets and flag lists
# ------------------------------------

# The first rule in the file had better be this one.  Don't put any above it.
# This lives here to allow makefile fragments to contain dependencies.
all: all.normal
.PHONY: all

#### host and target specific makefile fragments come in here.
@target_makefile_frag@
@alphaieee_frag@
@ospace_frag@
@host_makefile_frag@
###

# Flags to pass down to all sub-makes.
BASE_FLAGS_TO_PASS = [+ FOR flags_to_pass +]\
	"[+flag+]=$([+flag+])" [+ ENDFOR flags_to_pass +]\
	"CONFIG_SHELL=$(SHELL)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" 

# For any flags above that may contain shell code that varies from one
# target library to another.  When doing recursive invocations of the
# top-level Makefile, we don't want the outer make to evaluate them,
# so we pass these variables down unchanged.  They must not contain
# single nor double quotes.
RECURSE_FLAGS = \
	CXX_FOR_TARGET='$(CXX_FOR_TARGET_FOR_RECURSIVE_MAKE)' \
	RAW_CXX_FOR_TARGET='$(RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE)' \

# Flags to pass down to most sub-makes, in which we're building with
# the host environment.
EXTRA_HOST_FLAGS = \
	'AR=$(AR)' \
	'AS=$(AS)' \
	'CC=$(CC)' \
	'CXX=$(CXX)' \
	'DLLTOOL=$(DLLTOOL)' \
	'LD=$(LD)' \
	'NM=$(NM)' \
	'RANLIB=$(RANLIB)' \
	'WINDRES=$(WINDRES)'

FLAGS_TO_PASS = $(BASE_FLAGS_TO_PASS) $(EXTRA_HOST_FLAGS)

# Flags that are concerned with the location of the X11 include files
# and library files
#
# NOTE: until the top-level is getting the values via autoconf, it only
# causes problems to have this top-level Makefile overriding the autoconf-set
# values in child directories.  Only variables that don't conflict with
# autoconf'ed ones should be passed by X11_FLAGS_TO_PASS for now.
#
X11_FLAGS_TO_PASS = \
	'X11_EXTRA_CFLAGS=$(X11_EXTRA_CFLAGS)' \
	'X11_EXTRA_LIBS=$(X11_EXTRA_LIBS)'

# Flags to pass down to makes which are built with the target environment.
# The double $ decreases the length of the command line; the variables
# are set in BASE_FLAGS_TO_PASS, and the sub-make will expand them.
EXTRA_TARGET_FLAGS = \
	'AR=$$(AR_FOR_TARGET)' \
	'AS=$$(AS_FOR_TARGET)' \
	'CC=$$(CC_FOR_TARGET)' \
	'CFLAGS=$$(CFLAGS_FOR_TARGET)' \
	'CXX=$$(CXX_FOR_TARGET)' \
	'CXXFLAGS=$$(CXXFLAGS_FOR_TARGET)' \
	'DLLTOOL=$$(DLLTOOL_FOR_TARGET)' \
	'LD=$$(LD_FOR_TARGET)' \
	'LIBCFLAGS=$$(LIBCFLAGS_FOR_TARGET)' \
	'LIBCXXFLAGS=$$(LIBCXXFLAGS_FOR_TARGET)' \
	'NM=$$(NM_FOR_TARGET)' \
	'RANLIB=$$(RANLIB_FOR_TARGET)' \
	'WINDRES=$$(WINDRES_FOR_TARGET)'

TARGET_FLAGS_TO_PASS = $(BASE_FLAGS_TO_PASS) $(EXTRA_TARGET_FLAGS)

# Flags to pass down to gcc.  gcc builds a library, libgcc.a, so it
# unfortunately needs the native compiler and the target ar and
# ranlib.
# If any variables are added here, they must be added to do-*, below.
# The BUILD_* variables are a special case, which are used for the gcc
# cross-building scheme.
EXTRA_GCC_FLAGS = \
	'BUILD_PREFIX=$(BUILD_PREFIX)' \
	'BUILD_PREFIX_1=$(BUILD_PREFIX_1)' \
	"GCC_FOR_TARGET=$(GCC_FOR_TARGET)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"`echo 'LANGUAGES=$(LANGUAGES)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'STMP_FIXPROTO=$(STMP_FIXPROTO)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIMITS_H_TEST=$(LIMITS_H_TEST)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIBGCC2_CFLAGS=$(LIBGCC2_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIBGCC2_DEBUG_CFLAGS=$(LIBGCC2_DEBUG_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIBGCC2_INCLUDES=$(LIBGCC2_INCLUDES)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'STAGE1_CFLAGS=$(STAGE1_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'BOOT_CFLAGS=$(BOOT_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'BOOT_ADAFLAGS=$(BOOT_ADAFLAGS)' | sed -e s/.*=$$/XFOO=/`"

GCC_FLAGS_TO_PASS = $(BASE_FLAGS_TO_PASS) $(EXTRA_HOST_FLAGS) $(EXTRA_GCC_FLAGS)

.PHONY: configure-host
configure-host: maybe-configure-gcc [+
  FOR host_modules +] \
    maybe-configure-[+module+][+
  ENDFOR host_modules +]
.PHONY: configure-target
configure-target: [+
  FOR target_modules +] \
    maybe-configure-target-[+module+][+
  ENDFOR target_modules +]

# The target built for a native build.
.PHONY: all.normal
all.normal: @all_build_modules@ all-host all-target

.PHONY: all-host
all-host: maybe-all-gcc [+
  FOR host_modules +] \
    maybe-all-[+module+][+
  ENDFOR host_modules +]
.PHONY: all-target
all-target: [+
  FOR target_modules +] \
    maybe-all-target-[+module+][+
  ENDFOR target_modules +]

# Do a target for all the subdirectories.  A ``make do-X'' will do a
# ``make X'' in all subdirectories (because, in general, there is a
# dependency (below) of X upon do-X, a ``make X'' will also do this,
# but it may do additional work as well).
[+ FOR recursive_targets +]
.PHONY: do-[+make_target+]
do-[+make_target+]: [+make_target+]-host [+make_target+]-target

.PHONY: [+make_target+]-host
[+make_target+]-host: maybe-[+make_target+]-gcc [+
  FOR host_modules +] \
    maybe-[+make_target+]-[+module+][+
  ENDFOR host_modules +]

.PHONY: [+make_target+]-target
[+make_target+]-target: [+
  FOR target_modules +] \
    maybe-[+make_target+]-target-[+module+][+
  ENDFOR target_modules +]

# GCC, the eternal special case
.PHONY: maybe-[+make_target+]-gcc [+make_target+]-gcc
maybe-[+make_target+]-gcc:
[+make_target+]-gcc: [+
  FOR depend +]\
    [+depend+]-gcc [+
  ENDFOR depend +]
	@[ -f ./gcc/Makefile ] || exit 0; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	for flag in $(EXTRA_GCC_FLAGS); do \
	  eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	done; \
	echo "Doing [+make_target+] in gcc" ; \
	(cd gcc && \
	  $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
	          "CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	          "RANLIB=$${RANLIB}" \
	          "DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
	          [+make_target+]) \
	  || exit 1

# Host modules.
[+ FOR host_modules +]
.PHONY: maybe-[+make_target+]-[+module+] [+make_target+]-[+module+]
maybe-[+make_target+]-[+module+]:
[+ IF (match-value? = "missing" (get "make_target") ) +]
# [+module+] doesn't support [+make_target+].
[+make_target+]-[+module+]:
[+ ELSE +]
[+make_target+]-[+module+]: [+
  FOR depend +]\
    [+depend+]-[+module+] [+
  ENDFOR depend +]
	@[ -f ./[+module+]/Makefile ] || exit 0; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	for flag in $(EXTRA_HOST_FLAGS); do \
	  eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	done; \
	echo "Doing [+make_target+] in [+module+]" ; \
	(cd [+module+] && \
	  $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
	          "CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	          "RANLIB=$${RANLIB}" \
	          "DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
	          [+make_target+]) \
	  || exit 1
[+ ENDIF +]
[+ ENDFOR host_modules +]

# Target modules.
[+ FOR target_modules +]
.PHONY: maybe-[+make_target+]-target-[+module+] [+make_target+]-target-[+module+]
maybe-[+make_target+]-target-[+module+]:
[+ IF (match-value? = "missing" (get "make_target") ) +]
# [+module+] doesn't support [+make_target+].
[+make_target+]-target-[+module+]:
[+ ELSE +]
[+make_target+]-target-[+module+]: [+
  FOR depend +]\
    [+depend+]-target-[+module+] [+
  ENDFOR depend +]
	@[ -f $(TARGET_SUBDIR)/[+module+]/Makefile ] || exit 0 ; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	echo "Doing [+make_target+] in $(TARGET_SUBDIR)/[+module+]" ; \
	for flag in $(EXTRA_TARGET_FLAGS); do \
	  eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	done; \
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
	          "CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	          "RANLIB=$${RANLIB}" \
	          "DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
	          [+make_target+]) \
	  || exit 1
[+ ENDIF +]
[+ ENDFOR target_modules +]
[+ ENDFOR recursive_targets +]

# Here are the targets which correspond to the do-X targets.

.PHONY: info installcheck dvi install-info
.PHONY: clean distclean mostlyclean maintainer-clean realclean
.PHONY: local-clean local-distclean local-maintainer-clean
info: do-info
installcheck: do-installcheck
dvi: do-dvi

# Make sure makeinfo is built before we do a `make info', if we're
# in fact building texinfo.
do-info: maybe-all-texinfo

install-info: do-install-info dir.info
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	if [ -f dir.info ] ; then \
	  $(INSTALL_DATA) dir.info $(DESTDIR)$(infodir)/dir.info ; \
	else true ; fi

local-clean:
	-rm -f *.a TEMP errs core *.o *~ \#* TAGS *.E *.log

local-distclean:
	-rm -f Makefile config.status config.cache mh-frag mt-frag
	-rm -f multilib.out multilib.tmp maybedep.tmp serdep.tmp
	-if [ "$(TARGET_SUBDIR)" != "." ]; then \
	  rm -rf $(TARGET_SUBDIR); \
	else true; fi
	-rm -rf $(BUILD_SUBDIR)
	-rm -f texinfo/po/Makefile texinfo/po/Makefile.in texinfo/info/Makefile
	-rm -f texinfo/doc/Makefile texinfo/po/POTFILES
	-rmdir texinfo/doc texinfo/info texinfo/intl texinfo/lib 2>/dev/null
	-rmdir texinfo/makeinfo texinfo/po texinfo/util 2>/dev/null
	-rmdir fastjar gcc libiberty texinfo zlib 2>/dev/null

local-maintainer-clean:
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."

clean: do-clean local-clean
mostlyclean: do-mostlyclean local-clean
distclean: do-distclean local-clean local-distclean
maintainer-clean: local-maintainer-clean do-maintainer-clean local-clean 
maintainer-clean: local-distclean
realclean: maintainer-clean

# Extra dependency for clean-target, owing to the mixed nature of gcc
clean-target: clean-target-libgcc
clean-target-libgcc:
	test ! -d gcc/libgcc || \
	(cd gcc/libgcc && find . -type d -print) | \
	while read d; do rm -f gcc/$$d/libgcc.a || : ; done
	-rm -rf gcc/libgcc

# Check target.

.PHONY: check do-check
check: do-check

# Only include modules actually being configured and built.
do-check: maybe-check-gcc [+
  FOR host_modules +] \
    maybe-check-[+module+][+
  ENDFOR host_modules +][+
  FOR target_modules +] \
    maybe-check-target-[+module+][+
  ENDFOR target_modules +]

# Automated reporting of test results.

warning.log: build.log
	$(srcdir)/contrib/warn_summary build.log > $@

mail-report.log:
	if test x'$(BOOT_CFLAGS)' != x''; then \
	    BOOT_CFLAGS='$(BOOT_CFLAGS)'; export BOOT_CFLAGS; \
	fi; \
	$(srcdir)/contrib/test_summary -t >$@
	chmod +x $@
	echo If you really want to send e-mail, run ./$@ now

mail-report-with-warnings.log: warning.log
	if test x'$(BOOT_CFLAGS)' != x''; then \
	    BOOT_CFLAGS='$(BOOT_CFLAGS)'; export BOOT_CFLAGS; \
	fi; \
	$(srcdir)/contrib/test_summary -t -i warning.log >$@
	chmod +x $@
	echo If you really want to send e-mail, run ./$@ now

# Installation targets.

.PHONY: install uninstall
install: installdirs install-host install-target

.PHONY: install-host-nogcc
install-host-nogcc: [+
  FOR host_modules +] \
    maybe-install-[+module+][+
  ENDFOR host_modules +]

.PHONY: install-host
install-host: maybe-install-gcc [+
  FOR host_modules +] \
    maybe-install-[+module+][+
  ENDFOR host_modules +]

.PHONY: install-target
install-target: [+
  FOR target_modules +] \
    maybe-install-target-[+module+][+
  ENDFOR target_modules +]

uninstall:
	@echo "the uninstall target is not supported in this tree"

.PHONY: install.all
install.all: install-no-fixedincludes
	@if [ -f ./gcc/Makefile ] ; then \
		r=`${PWD_COMMAND}` ; export r ; \
		$(SET_LIB_PATH) \
		(cd ./gcc && \
		$(MAKE) $(FLAGS_TO_PASS) install-headers) ; \
	else \
		true ; \
	fi

# install-no-fixedincludes is used because Cygnus can not distribute
# the fixed header files.
.PHONY: install-no-fixedincludes
install-no-fixedincludes: installdirs install-host-nogcc \
	install-target gcc-no-fixedincludes

### other supporting targets

MAKEDIRS= \
	$(DESTDIR)$(prefix) \
	$(DESTDIR)$(exec_prefix)
.PHONY: installdirs
installdirs: mkinstalldirs
	$(SHELL) $(srcdir)/mkinstalldirs $(MAKEDIRS)

dir.info: do-install-info
	if [ -f $(srcdir)/texinfo/gen-info-dir ] ; then \
	  $(srcdir)/texinfo/gen-info-dir $(DESTDIR)$(infodir) $(srcdir)/texinfo/dir.info-template > dir.info.new ; \
	  mv -f dir.info.new dir.info ; \
	else true ; \
	fi

dist:
	@echo "Building a full distribution of this tree isn't done"
	@echo "via 'make dist'.  Check out the etc/ subdirectory" 

etags tags: TAGS

# Right now this just builds TAGS in each subdirectory.  emacs19 has the
# ability to use several tags files at once, so there is probably no need
# to combine them into one big TAGS file (like CVS 1.3 does).  We could
# (if we felt like it) have this Makefile write a piece of elisp which
# the user could load to tell emacs19 where all the TAGS files we just
# built are.
TAGS: do-TAGS

# --------------------------------------
# Modules which run on the build machine
# --------------------------------------
[+ FOR build_modules +]
.PHONY: configure-build-[+module+] maybe-configure-build-[+module+]
maybe-configure-build-[+module+]:
configure-build-[+module+]:
	@test ! -f $(BUILD_SUBDIR)/[+module+]/Makefile || exit 0; \
	$(SHELL) $(srcdir)/mkinstalldirs $(BUILD_SUBDIR)/[+module+] ; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	AR="$(AR_FOR_BUILD)"; export AR; \
	AS="$(AS_FOR_BUILD)"; export AS; \
	CC="$(CC_FOR_BUILD)"; export CC; \
	CFLAGS="$(CFLAGS_FOR_BUILD)"; export CFLAGS; \
	CONFIG_SHELL="$(SHELL)"; export CONFIG_SHELL; \
	CXX="$(CXX_FOR_BUILD)"; export CXX; \
	CXXFLAGS="$(CXXFLAGS_FOR_BUILD)"; export CXXFLAGS; \
	GCJ="$(GCJ_FOR_BUILD)"; export GCJ; \
	DLLTOOL="$(DLLTOOL_FOR_BUILD)"; export DLLTOOL; \
	LD="$(LD_FOR_BUILD)"; export LD; \
	LDFLAGS="$(LDFLAGS_FOR_BUILD)"; export LDFLAGS; \
	NM="$(NM_FOR_BUILD)"; export NM; \
	RANLIB="$(RANLIB_FOR_BUILD)"; export RANLIB; \
	WINDRES="$(WINDRES_FOR_BUILD)"; export WINDRES; \
	echo Configuring in $(BUILD_SUBDIR)/[+module+]; \
	cd "$(BUILD_SUBDIR)/[+module+]" || exit 1; \
	case $(srcdir) in \
	  /* | [A-Za-z]:[\\/]*) \
	    topdir=$(srcdir) ;; \
	  *) \
	    case "$(BUILD_SUBDIR)" in \
	      .) topdir="../$(srcdir)" ;; \
	      *) topdir="../../$(srcdir)" ;; \
	    esac ;; \
	esac; \
	if [ "$(srcdir)" = "." ] ; then \
	  if [ "$(BUILD_SUBDIR)" != "." ] ; then \
	    if $(SHELL) $$s/symlink-tree $${topdir}/[+module+] "no-such-file" ; then \
	      if [ -f Makefile ]; then \
	        if $(MAKE) distclean; then \
	          true; \
	        else \
	          exit 1; \
	        fi; \
	      else \
	        true; \
	      fi; \
	    else \
	      exit 1; \
	    fi; \
	  else \
	    true; \
	  fi; \
	  srcdiroption="--srcdir=."; \
	  libsrcdir="."; \
	else \
	  srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	  libsrcdir="$$s/[+module+]"; \
	fi; \
	rm -f no-such-file || : ; \
	CONFIG_SITE=no-such-file $(SHELL) $${libsrcdir}/configure \
	  $(BUILD_CONFIGARGS) $${srcdiroption} \
	  --with-build-subdir="$(BUILD_SUBDIR)" \
	  || exit 1

.PHONY: all-build-[+module+] maybe-all-build-[+module+]
maybe-all-build-[+module+]:
all-build-[+module+]: configure-build-[+module+]
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	(cd $(BUILD_SUBDIR)/[+module+] && $(MAKE) all)
[+ ENDFOR build_modules +]

# --------------------------------------
# Modules which run on the host machine
# --------------------------------------
[+ FOR host_modules +]
.PHONY: configure-[+module+] maybe-configure-[+module+]
maybe-configure-[+module+]:
configure-[+module+]:
	@test ! -f [+module+]/Makefile || exit 0; \
	[ -d [+module+] ] || mkdir [+module+]; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	CC="$(CC)"; export CC; \
	CFLAGS="$(CFLAGS)"; export CFLAGS; \
	CONFIG_SHELL="$(SHELL)"; export CONFIG_SHELL; \
	CXX="$(CXX)"; export CXX; \
	CXXFLAGS="$(CXXFLAGS)"; export CXXFLAGS; \
	AR="$(AR)"; export AR; \
	AS="$(AS)"; export AS; \
	CC_FOR_BUILD="$(CC_FOR_BUILD)"; export CC_FOR_BUILD; \
	DLLTOOL="$(DLLTOOL)"; export DLLTOOL; \
	LD="$(LD)"; export LD; \
	NM="$(NM)"; export NM; \
	RANLIB="$(RANLIB)"; export RANLIB; \
	WINDRES="$(WINDRES)"; export WINDRES; \
	OBJCOPY="$(OBJCOPY)"; export OBJCOPY; \
	OBJDUMP="$(OBJDUMP)"; export OBJDUMP; \
	echo Configuring in [+module+]; \
	cd [+module+] || exit 1; \
	case $(srcdir) in \
	  \.) \
	    srcdiroption="--srcdir=."; \
	    libsrcdir=".";; \
	  /* | [A-Za-z]:[\\/]*) \
	    srcdiroption="--srcdir=$(srcdir)/[+module+]"; \
	    libsrcdir="$$s/[+module+]";; \
	  *) \
	    srcdiroption="--srcdir=../$(srcdir)/[+module+]"; \
	    libsrcdir="$$s/[+module+]";; \
	esac; \
	$(SHELL) $${libsrcdir}/configure \
	  $(HOST_CONFIGARGS) $${srcdiroption} \
	  || exit 1

.PHONY: all-[+module+] maybe-all-[+module+]
maybe-all-[+module+]:
all-[+module+]: configure-[+module+]
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	(cd [+module+] && $(MAKE) $(FLAGS_TO_PASS)[+ 
	  IF with_x 
	    +] $(X11_FLAGS_TO_PASS)[+ 
	  ENDIF with_x +] all)

.PHONY: check-[+module+] maybe-check-[+module+]
maybe-check-[+module+]:
[+ IF no_check +]
check-[+module+]:
[+ ELIF no_check_cross +]
# This module is only tested in a native toolchain.
check-[+module+]:
	@if [ '$(host)' = '$(target)' ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd [+module+] && $(MAKE) $(FLAGS_TO_PASS)[+ 
	    IF with_x 
	      +] $(X11_FLAGS_TO_PASS)[+ 
	    ENDIF with_x +] check); \
	fi
[+ ELSE check +]
check-[+module+]:
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	(cd [+module+] && $(MAKE) $(FLAGS_TO_PASS)[+ 
	  IF with_x 
	    +] $(X11_FLAGS_TO_PASS)[+ 
	  ENDIF with_x +] check)
[+ ENDIF no_check +]

.PHONY: install-[+module+] maybe-install-[+module+]
maybe-install-[+module+]:
[+ IF no_install +]
install-[+module+]:
[+ ELSE install +]
install-[+module+]: installdirs
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	(cd [+module+] && $(MAKE) $(FLAGS_TO_PASS)[+ 
	  IF with_x 
	    +] $(X11_FLAGS_TO_PASS)[+ 
	  ENDIF with_x +] install)
[+ ENDIF no_install +]
[+ ENDFOR host_modules +]

# ---------------------------------------
# Modules which run on the target machine
# ---------------------------------------
[+ FOR target_modules +]
.PHONY: configure-target-[+module+] maybe-configure-target-[+module+]
maybe-configure-target-[+module+]:

# There's only one multilib.out.  Cleverer subdirs shouldn't need it copied.
$(TARGET_SUBDIR)/[+module+]/multilib.out: multilib.out
	$(SHELL) $(srcdir)/mkinstalldirs $(TARGET_SUBDIR)/[+module+] ; \
	rm -f $(TARGET_SUBDIR)/[+module+]/Makefile || : ; \
	cp multilib.out $(TARGET_SUBDIR)/[+module+]/multilib.out

configure-target-[+module+]: $(TARGET_SUBDIR)/[+module+]/multilib.out
	@test ! -f $(TARGET_SUBDIR)/[+module+]/Makefile || exit 0; \
	$(SHELL) $(srcdir)/mkinstalldirs $(TARGET_SUBDIR)/[+module+] ; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	AR="$(AR_FOR_TARGET)"; export AR; \
	AS="$(AS_FOR_TARGET)"; export AS; \
	CC="$(CC_FOR_TARGET)"; export CC; \
	CFLAGS="$(CFLAGS_FOR_TARGET)"; export CFLAGS; \
	CONFIG_SHELL="$(SHELL)"; export CONFIG_SHELL; \
	CPPFLAGS="$(CFLAGS_FOR_TARGET)"; export CPPFLAGS; \[+ 
IF raw_cxx +]
	CXX_FOR_TARGET="$(RAW_CXX_FOR_TARGET)"; export CXX_FOR_TARGET; \
	CXX="$(RAW_CXX_FOR_TARGET)"; export CXX; \[+ 
ELSE normal_cxx +]
	CXX="$(CXX_FOR_TARGET)"; export CXX; \[+ 
ENDIF raw_cxx +]
	CXXFLAGS="$(CXXFLAGS_FOR_TARGET)"; export CXXFLAGS; \
	GCJ="$(GCJ_FOR_TARGET)"; export GCJ; \
	DLLTOOL="$(DLLTOOL_FOR_TARGET)"; export DLLTOOL; \
	LD="$(LD_FOR_TARGET)"; export LD; \
	LDFLAGS="$(LDFLAGS_FOR_TARGET)"; export LDFLAGS; \
	NM="$(NM_FOR_TARGET)"; export NM; \
	RANLIB="$(RANLIB_FOR_TARGET)"; export RANLIB; \
	WINDRES="$(WINDRES_FOR_TARGET)"; export WINDRES; \
	SET_GCC_LIB_PATH_CMD="@SET_GCC_LIB_PATH@"; export SET_GCC_LIB_PATH_CMD; \
	@SET_GCC_LIB_PATH@ \
	echo Configuring in $(TARGET_SUBDIR)/[+module+]; \
	cd "$(TARGET_SUBDIR)/[+module+]" || exit 1; \
	case $(srcdir) in \
	  /* | [A-Za-z]:[\\/]*) \
	    topdir=$(srcdir) ;; \
	  *) \
	    case "$(TARGET_SUBDIR)" in \
	      .) topdir="../$(srcdir)" ;; \
	      *) topdir="../../$(srcdir)" ;; \
	    esac ;; \
	esac; \[+ IF stage +]
	if [ "$(srcdir)" = "." ] ; then \
	  if [ "$(TARGET_SUBDIR)" != "." ] ; then \
	    if $(SHELL) $$s/symlink-tree $${topdir}/[+module+] "no-such-file" ; then \
	      if [ -f Makefile ]; then \
	        if $(MAKE) distclean; then \
	          true; \
	        else \
	          exit 1; \
	        fi; \
	      else \
	        true; \
	      fi; \
	    else \
	      exit 1; \
	    fi; \
	  else \
	    true; \
	  fi; \
	  srcdiroption="--srcdir=."; \
	  libsrcdir="."; \
	else \[+ ENDIF stage +]
	  srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	  libsrcdir="$$s/[+module+]"; \[+ IF stage +]
	fi; \[+ ENDIF stage +]
	rm -f no-such-file || : ; \
	CONFIG_SITE=no-such-file $(SHELL) $${libsrcdir}/configure \
	  $(TARGET_CONFIGARGS) $${srcdiroption} \
	  --with-target-subdir="$(TARGET_SUBDIR)" \
	  || exit 1

.PHONY: all-target-[+module+] maybe-all-target-[+module+]
maybe-all-target-[+module+]:
all-target-[+module+]: configure-target-[+module+]
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(TARGET_FLAGS_TO_PASS) [+
	    IF raw_cxx 
	  +] 'CXX=$$(RAW_CXX_FOR_TARGET)' 'CXX_FOR_TARGET=$$(RAW_CXX_FOR_TARGET)' [+ 
	    ENDIF raw_cxx 
	  +] all)

.PHONY: check-target-[+module+] maybe-check-target-[+module+]
maybe-check-target-[+module+]:
[+ IF no_check +]
# Dummy target for uncheckable module.
check-target-[+module+]:
[+ ELSE check +]
check-target-[+module+]:
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(TARGET_FLAGS_TO_PASS) [+
	    IF raw_cxx 
	      +] 'CXX=$$(RAW_CXX_FOR_TARGET)' 'CXX_FOR_TARGET=$$(RAW_CXX_FOR_TARGET)' [+ 
	    ENDIF raw_cxx 
	  +] check)
[+ ENDIF no_check +]

.PHONY: install-target-[+module+] maybe-install-target-[+module+]
maybe-install-target-[+module+]:
[+ IF no_install +]
# Dummy target for uninstallable.
install-target-[+module+]:
[+ ELSE install +]
install-target-[+module+]: installdirs
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(TARGET_FLAGS_TO_PASS) install)
[+ ENDIF no_install +]
[+ ENDFOR target_modules +]

# ----------
# GCC module
# ----------

# Unfortunately, while gcc _should_ be a host module,
# libgcc is a target module, and gen* programs are
# build modules.  So GCC is a sort of hybrid.

# gcc is the only module which uses GCC_FLAGS_TO_PASS.
# Don't use shared host config.cache, as it will confuse later
# directories; GCC wants slightly different values for some
# precious variables.  *sigh*
.PHONY: configure-gcc maybe-configure-gcc
maybe-configure-gcc:
configure-gcc:
	@test ! -f gcc/Makefile || exit 0; \
	[ -d gcc ] || mkdir gcc; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	CC="$(CC)"; export CC; \
	CFLAGS="$(CFLAGS)"; export CFLAGS; \
	CONFIG_SHELL="$(SHELL)"; export CONFIG_SHELL; \
	CXX="$(CXX)"; export CXX; \
	CXXFLAGS="$(CXXFLAGS)"; export CXXFLAGS; \
	TOPLEVEL_CONFIGURE_ARGUMENTS="$(TOPLEVEL_CONFIGURE_ARGUMENTS)"; export TOPLEVEL_CONFIGURE_ARGUMENTS; \
	AR="$(AR)"; export AR; \
	AS="$(AS)"; export AS; \
	CC_FOR_BUILD="$(CC_FOR_BUILD)"; export CC_FOR_BUILD; \
	DLLTOOL="$(DLLTOOL)"; export DLLTOOL; \
	LD="$(LD)"; export LD; \
	NM="$(NM)"; export NM; \
	RANLIB="$(RANLIB)"; export RANLIB; \
	WINDRES="$(WINDRES)"; export WINDRES; \
	OBJCOPY="$(OBJCOPY)"; export OBJCOPY; \
	OBJDUMP="$(OBJDUMP)"; export OBJDUMP; \
	SET_GCC_LIB_PATH_CMD="@SET_GCC_LIB_PATH@"; export SET_GCC_LIB_PATH_CMD; \
	@SET_GCC_LIB_PATH@ \
	echo Configuring in gcc; \
	cd gcc || exit 1; \
	case $(srcdir) in \
	  \.) \
	    srcdiroption="--srcdir=."; \
	    libsrcdir=".";; \
	  /* | [A-Za-z]:[\\/]*) \
	    srcdiroption="--srcdir=$(srcdir)/gcc"; \
	    libsrcdir="$$s/gcc";; \
	  *) \
	    srcdiroption="--srcdir=../$(srcdir)/gcc"; \
	    libsrcdir="$$s/gcc";; \
	esac; \
	$(SHELL) $${libsrcdir}/configure \
	  $(HOST_CONFIGARGS) $${srcdiroption} \
	  || exit 1

# Don't 'make all' in gcc if it's already been made by 'bootstrap'; that
# causes trouble.  This wart will be fixed eventually by moving
# the bootstrap behavior to this file.
.PHONY: all-gcc maybe-all-gcc
maybe-all-gcc:
all-gcc: configure-gcc
	@if [ -f gcc/stage_last ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) quickstrap); \
	else \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) all); \
	fi

# Building GCC uses some tools for rebuilding "source" files
# like texinfo, bison/byacc, etc.  So we must depend on those.
#
# While building GCC, it may be necessary to run various target
# programs like the assembler, linker, etc.  So we depend on
# those too.
#
# In theory, on an SMP all those dependencies can be resolved
# in parallel.
#
GCC_STRAP_TARGETS = bootstrap bootstrap-lean bootstrap2 bootstrap2-lean bootstrap3 bootstrap3-lean bootstrap4 bootstrap4-lean bubblestrap quickstrap cleanstrap restrap
.PHONY: $(GCC_STRAP_TARGETS)
$(GCC_STRAP_TARGETS): all-bootstrap configure-gcc
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	echo "Bootstrapping the compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) $@
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	case "$@" in \
	  *bootstrap4-lean ) \
	    msg="Comparing stage3 and stage4 of the compiler"; \
	    compare=compare3-lean ;; \
	  *bootstrap4 ) \
	    msg="Comparing stage3 and stage4 of the compiler"; \
	    compare=compare3 ;; \
	  *-lean ) \
	    msg="Comparing stage2 and stage3 of the compiler"; \
	    compare=compare-lean ;; \
	  * ) \
	    msg="Comparing stage2 and stage3 of the compiler"; \
	    compare=compare ;; \
	esac; \
	$(SET_LIB_PATH) \
	echo "$$msg"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) $$compare
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS) all

profiledbootstrap: all-bootstrap configure-gcc
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	echo "Bootstrapping training compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) stageprofile_build
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	echo "Building feedback based compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) stagefeedback_build
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS) all

.PHONY: cross
cross: all-texinfo all-bison all-byacc all-binutils all-gas all-ld
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	echo "Building the C and C++ compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) LANGUAGES="c c++"
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS) \
	  LANGUAGES="c c++" all

.PHONY: check-gcc maybe-check-gcc
maybe-check-gcc:
check-gcc:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) check); \
	else \
	  true; \
	fi

.PHONY: check-gcc-c++
check-gcc-c++:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) check-c++); \
	else \
	  true; \
	fi

.PHONY: check-c++
check-c++: check-target-libstdc++-v3 check-gcc-c++

.PHONY: install-gcc maybe-install-gcc
maybe-install-gcc:
install-gcc:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) install); \
	else \
	  true; \
	fi

# Install the gcc headers files, but not the fixed include files,
# which Cygnus is not allowed to distribute.  This rule is very
# dependent on the workings of the gcc Makefile.in.
.PHONY: gcc-no-fixedincludes
gcc-no-fixedincludes:
	@if [ -f ./gcc/Makefile ]; then \
	  rm -rf gcc/tmp-include; \
	  mv gcc/include gcc/tmp-include 2>/dev/null; \
	  mkdir gcc/include; \
	  cp $(srcdir)/gcc/gsyslimits.h gcc/include/syslimits.h; \
	  touch gcc/stmp-fixinc gcc/include/fixed; \
	  rm -f gcc/stmp-headers gcc/stmp-int-hdrs; \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	  $(SET_LIB_PATH) \
	  (cd ./gcc && \
	   $(MAKE) $(GCC_FLAGS_TO_PASS) install); \
	  rm -rf gcc/include; \
	  mv gcc/tmp-include gcc/include 2>/dev/null; \
	else true; fi

# --------------------------------------
# Dependencies between different modules
# --------------------------------------

# There are two types of dependencies here: 'hard' dependencies, where one
# module simply won't build without the other; and 'soft' dependencies, where
# if the depended-on module is missing, the depending module will do without
# or find a substitute somewhere (perhaps installed).  Soft dependencies
# are specified by depending on a 'maybe-' target.  If you're not sure,
# it's safer to use a soft dependency.

# Host modules specific to gcc.
# GCC needs to identify certain tools.
# GCC also needs the information exported by the intl configure script.
configure-gcc: maybe-configure-intl maybe-configure-binutils maybe-configure-gas maybe-configure-ld maybe-configure-bison maybe-configure-flex
all-gcc: maybe-all-libiberty maybe-all-intl maybe-all-bison maybe-all-byacc maybe-all-binutils maybe-all-gas maybe-all-ld maybe-all-zlib
# This is a slightly kludgy method of getting dependencies on 
# all-build-libiberty correct; it would be better to build it every time.
all-gcc: maybe-all-build-libiberty
all-bootstrap: maybe-all-libiberty maybe-all-intl maybe-all-texinfo maybe-all-bison maybe-all-byacc maybe-all-binutils maybe-all-gas maybe-all-ld maybe-all-zlib

# Host modules specific to gdb.
# GDB needs to know that the simulator is being built.
configure-gdb: maybe-configure-itcl maybe-configure-tcl maybe-configure-tk maybe-configure-sim
GDB_TK = @GDB_TK@
all-gdb: maybe-all-libiberty maybe-all-opcodes maybe-all-bfd maybe-all-mmalloc maybe-all-readline maybe-all-bison maybe-all-byacc maybe-all-sim $(gdbnlmrequirements) $(GDB_TK)
install-gdb: maybe-install-tcl maybe-install-tk maybe-install-itcl maybe-install-tix maybe-install-libgui
configure-libgui: maybe-configure-tcl maybe-configure-tk
all-libgui: maybe-all-tcl maybe-all-tk maybe-all-itcl

# Host modules specific to binutils.
configure-bfd: configure-libiberty
all-bfd: maybe-all-libiberty maybe-all-intl
all-binutils: maybe-all-libiberty maybe-all-opcodes maybe-all-bfd maybe-all-flex maybe-all-bison maybe-all-byacc maybe-all-intl
# We put install-opcodes before install-binutils because the installed
# binutils might be on PATH, and they might need the shared opcodes
# library.
install-binutils: maybe-install-opcodes
# libopcodes depends on libbfd
install-opcodes: maybe-install-bfd
all-gas: maybe-all-libiberty maybe-all-opcodes maybe-all-bfd maybe-all-intl
all-gprof: maybe-all-libiberty maybe-all-bfd maybe-all-opcodes maybe-all-intl
all-ld: maybe-all-libiberty maybe-all-bfd maybe-all-opcodes maybe-all-bison maybe-all-byacc maybe-all-flex maybe-all-intl
all-opcodes: maybe-all-bfd maybe-all-libiberty

# Other host modules in the 'src' repository.
all-dejagnu: maybe-all-tcl maybe-all-expect maybe-all-tk
configure-expect: maybe-configure-tcl maybe-configure-tk
all-expect: maybe-all-tcl maybe-all-tk
configure-itcl: maybe-configure-tcl maybe-configure-tk
all-itcl: maybe-all-tcl maybe-all-tk
# We put install-tcl before install-itcl because itcl wants to run a
# program on installation which uses the Tcl libraries.
install-itcl: maybe-install-tcl
all-sid: maybe-all-libiberty maybe-all-bfd maybe-all-opcodes maybe-all-tcl maybe-all-tk
install-sid: maybe-install-tcl maybe-install-tk
all-sim: maybe-all-libiberty maybe-all-bfd maybe-all-opcodes maybe-all-readline maybe-configure-gdb
configure-tk: maybe-configure-tcl
all-tk: maybe-all-tcl
configure-tix: maybe-configure-tcl maybe-configure-tk
all-tix: maybe-all-tcl maybe-all-tk
all-texinfo: maybe-all-libiberty

# Other host modules.  Warning, these are not well tested.
all-autoconf: maybe-all-m4 maybe-all-texinfo
all-automake: maybe-all-m4 maybe-all-texinfo
all-bison: maybe-all-texinfo
all-diff: maybe-all-libiberty
all-fastjar: maybe-all-zlib maybe-all-libiberty
all-fileutils: maybe-all-libiberty
all-flex: maybe-all-libiberty maybe-all-bison maybe-all-byacc
all-gzip: maybe-all-libiberty
all-hello: maybe-all-libiberty
all-m4: maybe-all-libiberty maybe-all-texinfo
all-make: maybe-all-libiberty maybe-all-intl
all-patch: maybe-all-libiberty
all-prms: maybe-all-libiberty
all-recode: maybe-all-libiberty
all-sed: maybe-all-libiberty
all-send-pr: maybe-all-prms
all-tar: maybe-all-libiberty
all-uudecode: maybe-all-libiberty

ALL_GCC = maybe-all-gcc
ALL_GCC_C = $(ALL_GCC) maybe-all-target-newlib maybe-all-target-libgloss
ALL_GCC_CXX = $(ALL_GCC_C) maybe-all-target-libstdc++-v3

# Target modules specific to gcc.
configure-target-boehm-gc: $(ALL_GCC_C) maybe-configure-target-qthreads
configure-target-fastjar: maybe-configure-target-zlib
all-target-fastjar: maybe-all-target-zlib maybe-all-target-libiberty
configure-target-libf2c: $(ALL_GCC_C)
all-target-libf2c: maybe-all-target-libiberty
configure-target-libffi: $(ALL_GCC_C) 
configure-target-libjava: $(ALL_GCC_C) maybe-configure-target-zlib maybe-configure-target-boehm-gc maybe-configure-target-qthreads maybe-configure-target-libffi
all-target-libjava: maybe-all-fastjar maybe-all-target-zlib maybe-all-target-boehm-gc maybe-all-target-qthreads maybe-all-target-libffi
configure-target-libobjc: $(ALL_GCC_C)
all-target-libobjc: maybe-all-target-libiberty
configure-target-libstdc++-v3: $(ALL_GCC_C)
all-target-libstdc++-v3: maybe-all-target-libiberty
configure-target-zlib: $(ALL_GCC_C)

# Target modules in the 'src' repository.
configure-target-examples: $(ALL_GCC_C)
configure-target-libgloss: $(ALL_GCC)
all-target-libgloss: maybe-configure-target-newlib
configure-target-libiberty: $(ALL_GCC)
configure-target-libtermcap: $(ALL_GCC_C)
configure-target-newlib: $(ALL_GCC)
configure-target-rda: $(ALL_GCC_C)
configure-target-winsup: $(ALL_GCC_C)
all-target-winsup: maybe-all-target-libiberty maybe-all-target-libtermcap

# Other target modules.  Warning, these are not well tested.
configure-target-gperf: $(ALL_GCC_CXX)
all-target-gperf: maybe-all-target-libiberty maybe-all-target-libstdc++-v3
configure-target-qthreads: $(ALL_GCC_C)

# Dependencies of maybe-foo on foo.  These are used because, for example,
# all-gcc only depends on all-gas if gas is present and being configured.
@maybe_dependencies@

# Serialization dependencies.  Host configures don't work well in parallel to
# each other, due to contention over config.cache.  Target configures and 
# build configures are similar.
@serialization_dependencies@

# --------------------------------
# Regenerating top level configury
# --------------------------------

# Multilib.out tells target dirs what multilibs they should build.
# There is really only one copy.  We use the 'timestamp' method to
# work around various timestamp bugs on some systems.
# We use move-if-change so that it's only considered updated when it
# actually changes, because it has to depend on a phony target.
multilib.out: maybe-all-gcc
	@r=`${PWD_COMMAND}`; export r; \
	echo "Checking multilib configuration..."; \
	$(CC_FOR_TARGET) --print-multi-lib > multilib.tmp 2> /dev/null ; \
	$(SHELL) $(srcdir)/move-if-change multilib.tmp multilib.out ; \

# Rebuilding Makefile.in, using autogen.
AUTOGEN = autogen
$(srcdir)/Makefile.in: @MAINT@ $(srcdir)/Makefile.tpl $(srcdir)/Makefile.def
	cd $(srcdir) && $(AUTOGEN) Makefile.def

# Rebuilding Makefile.
Makefile: $(srcdir)/Makefile.in config.status
	CONFIG_FILES=$@ CONFIG_HEADERS= $(SHELL) ./config.status

config.status: configure $(gcc_version_trigger)
	CONFIG_SHELL="$(SHELL)" $(SHELL) ./config.status --recheck

# Rebuilding configure.
AUTOCONF = autoconf
$(srcdir)/configure: @MAINT@ $(srcdir)/configure.in $(srcdir)/config/acx.m4
	cd $(srcdir) && $(AUTOCONF)

# ------------------------------
# Special directives to GNU Make
# ------------------------------

# Don't pass command-line variables to submakes.
.NOEXPORT:
MAKEOVERRIDES=

# end of Makefile.in
