[+ AutoGen5 template -*- Mode: Makefile -*-
in
+]

# Makefile.in is generated from Makefile.tpl by 'autogen Makefile.def'.
#
# Makefile for directory with subdirs to build.
#   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
#   1999, 2000, 2001, 2002 Free Software Foundation
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

# Tell GNU make 3.79 not to run the top level in parallel.  This 
# prevents contention for $builddir/$target/config.cache, as well
# as minimizing scatter in file system caches.
NOTPARALLEL = .NOTPARALLEL
$(NOTPARALLEL):

VPATH=@srcdir@
links=@configlinks@

build_alias=@build_alias@
build_cpu=@build_cpu@
build_vendor=@build_vendor@
build_os=@build_os@
build_canonical=@build_cpu@-@build_vendor@-@build_os@
host_alias=@host_alias@
host_cpu=@host_cpu@
host_vendor=@host_vendor@
host_os=@host_os@
host_canonical=@host_cpu@-@host_vendor@-@host_os@
target_alias=@target_alias@
target_cpu=@target_cpu@
target_vendor=@target_vendor@
target_os=@target_os@
target_canonical=@target_cpu@-@target_vendor@-@target_os@

enable_shared = @enable_shared@
enable_threads = @enable_threads@
enable_version_specific_runtime_libs = @enable_version_specific_runtime_libs@
# The file containing GCC's version number.
gcc_version_trigger = @gcc_version_trigger@
gcc_version = @gcc_version@

srcdir = @srcdir@

prefix = @prefix@
exec_prefix = @exec_prefix@

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
gxx_include_dir=@gxx_include_dir@

tooldir = @tooldir@
build_tooldir = @build_tooldir@

program_transform_name = @program_transform_name@

man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
# Directory in which the compiler finds executables, libraries, etc.
libsubdir = $(libdir)/gcc-lib/$(target_alias)/$(gcc_version)
GDB_NLM_DEPS = 

SHELL = @config_shell@

# pwd command to use.  Allow user to override default by setting PWDCMD in
# the environment to account for automounters.  The make variable must not
# be called PWDCMD, otherwise the value set here is passed to make
# subprocesses and overrides the setting from the user's environment.
PWD = $${PWDCMD-pwd}

# INSTALL_PROGRAM_ARGS is changed by configure.in to use -x for a
# cygwin host.
INSTALL_PROGRAM_ARGS =

INSTALL = $(SHELL) $$s/install-sh -c
INSTALL_PROGRAM = $(INSTALL) $(INSTALL_PROGRAM_ARGS)
INSTALL_SCRIPT = $(INSTALL)
INSTALL_DATA = $(INSTALL) -m 644

AS = @AS@
AR = @AR@
AR_FLAGS = rc
CC = @CC@

# Special variables passed down in EXTRA_GCC_FLAGS.  They are defined
# here so that they can be overridden by Makefile fragments.
BUILD_CC = $(CC_FOR_BUILD)
BUILD_PREFIX = @BUILD_PREFIX@
BUILD_PREFIX_1 = @BUILD_PREFIX_1@

CFLAGS = @CFLAGS@
CXXFLAGS = @CXXFLAGS@

LDFLAGS = 
LIBCFLAGS = $(CFLAGS)
CFLAGS_FOR_BUILD = $(CFLAGS)
# During gcc bootstrap, if we use some random cc for stage1 then
# CFLAGS will be just -g.  We want to ensure that TARGET libraries
# (which we know are built with gcc) are built with optimizations so
# prepend -O2 when setting CFLAGS_FOR_TARGET.
CFLAGS_FOR_TARGET = -O2 $(CFLAGS)
LDFLAGS_FOR_TARGET = 
LIBCFLAGS_FOR_TARGET = $(CFLAGS_FOR_TARGET)
PICFLAG = 
PICFLAG_FOR_TARGET = 

CXX = @CXX@

# Use -O2 to stress test the compiler.
LIBCXXFLAGS = $(CXXFLAGS) -fno-implicit-templates
CXXFLAGS_FOR_TARGET = $(CXXFLAGS)
LIBCXXFLAGS_FOR_TARGET = $(CXXFLAGS_FOR_TARGET) -fno-implicit-templates

DLLTOOL = @DLLTOOL@
WINDRES = @WINDRES@

NM = @NM@

LD = @LD@

# These values are substituted by configure.
DEFAULT_YACC = @DEFAULT_YACC@
DEFAULT_LEX = @DEFAULT_LEX@
DEFAULT_M4 = @DEFAULT_M4@

BISON=@BISON@
USUAL_BISON = `if [ -f $$r/bison/bison ] ; then \
	    echo $$r/bison/bison -L $$s/bison/ ; \
	 else \
	    echo bison ; \
	 fi`

YACC=@YACC@
USUAL_YACC = `if [ -f $$r/bison/bison ] ; then \
	    echo $$r/bison/bison -y -L $$s/bison/ ; \
	elif [ -f $$r/byacc/byacc ] ; then \
	    echo $$r/byacc/byacc ; \
	else \
	    echo ${DEFAULT_YACC} ; \
	fi`

LEX=@LEX@
USUAL_LEX = `if [ -f $$r/flex/flex ] ; \
	then echo $$r/flex/flex ; \
	else echo ${DEFAULT_LEX} ; fi`

M4 = `if [ -f $$r/m4/m4 ] ; \
	then echo $$r/m4/m4 ; \
	else echo ${DEFAULT_M4} ; fi`

# For an installed makeinfo, we require it to be from texinfo 4 or
# higher, else we use the "missing" dummy.
MAKEINFO=@MAKEINFO@
USUAL_MAKEINFO = `if [ -f $$r/texinfo/makeinfo/makeinfo ] ; \
	then echo $$r/texinfo/makeinfo/makeinfo ; \
	else if (makeinfo --version \
	  | egrep 'texinfo[^0-9]*([1-3][0-9]|[4-9])') >/dev/null 2>&1; \
        then echo makeinfo; else echo $$s/missing makeinfo; fi; fi`

# This just becomes part of the MAKEINFO definition passed down to
# sub-makes.  It lets flags be given on the command line while still
# using the makeinfo from the object tree.
MAKEINFOFLAGS =

EXPECT = `if [ -f $$r/expect/expect ] ; \
	then echo $$r/expect/expect ; \
	else echo expect ; fi`

RUNTEST = `if [ -f $$s/dejagnu/runtest ] ; \
	then echo $$s/dejagnu/runtest ; \
	else echo runtest ; fi`


# compilers to use to create programs which must be run in the build
# environment.
CC_FOR_BUILD = @CC_FOR_BUILD@
CXX_FOR_BUILD = $(CXX)

SUBDIRS = @configdirs@

# This is set by the configure script to the list of directories which
# should be built using the target tools.
TARGET_CONFIGDIRS = @target_configdirs@

# Target libraries are put under this directory:
# Changed by configure to $(target_alias) if cross.
TARGET_SUBDIR = @target_subdir@

BUILD_CONFIGDIRS = libiberty
BUILD_SUBDIR = @build_subdir@

# This is set by the configure script to the arguments to use when configuring
# directories built for the build system.
BUILD_CONFIGARGS = @build_configargs@

# This is set by the configure script to the arguments to use when configuring
# directories built for the host system.
HOST_CONFIGARGS = @host_configargs@

# This is set by the configure script to the arguments to use when configuring
# directories built for the target.
TARGET_CONFIGARGS = @target_configargs@

# This is set by configure to REALLY_SET_LIB_PATH if --enable-shared
# was used.
SET_LIB_PATH = @SET_LIB_PATH@

# This is the name of the environment variable used for the path to
# the libraries.  This may be changed by configure.in.
RPATH_ENVVAR = @RPATH_ENVVAR@

# This is the list of directories that may be needed in RPATH_ENVVAR
# so that programs built for the host machine work.
HOST_LIB_PATH = $$r/bfd:$$r/opcodes

# This is the list of directories that may be needed in RPATH_ENVVAR
# so that prorgams built for the target machine work.
TARGET_LIB_PATH = $$r/$(TARGET_SUBDIR)/libstdc++-v3/src/.libs:

# configure.in sets SET_LIB_PATH to this if --enable-shared was used.
# Some platforms don't like blank entries, so we remove duplicate,
# leading and trailing colons.
REALLY_SET_LIB_PATH = \
  $(RPATH_ENVVAR)=`echo "$(HOST_LIB_PATH):$(TARGET_LIB_PATH):$$$(RPATH_ENVVAR)" | sed 's,::*,:,g;s,^:*,,;s,:*$$,,'`; export $(RPATH_ENVVAR);

# Should be substed by configure.in
FLAGS_FOR_TARGET = @FLAGS_FOR_TARGET@
CC_FOR_TARGET = @CC_FOR_TARGET@
CXX_FOR_TARGET = @CXX_FOR_TARGET@
RAW_CXX_FOR_TARGET = @RAW_CXX_FOR_TARGET@
CXX_FOR_TARGET_FOR_RECURSIVE_MAKE = @CXX_FOR_TARGET_FOR_RECURSIVE_MAKE@
RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE = @RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE@
GCJ_FOR_TARGET = @GCJ_FOR_TARGET@

# If GCC_FOR_TARGET is not overriden on the command line, then this
# variable is passed down to the gcc Makefile, where it is used to
# build libgcc2.a.  We define it here so that it can itself be
# overridden on the command line.
GCC_FOR_TARGET=@GCC_FOR_TARGET@
USUAL_GCC_FOR_TARGET = $(STAGE_CC_WRAPPER) $$r/gcc/xgcc -B$$r/gcc/ $(FLAGS_FOR_TARGET)

AS_FOR_TARGET=@AS_FOR_TARGET@
USUAL_AS_FOR_TARGET = ` \
  if [ -f $$r/gas/as-new ] ; then \
    echo $$r/gas/as-new ; \
  elif [ -f $$r/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=as ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      echo $(AS); \
    else \
       t='$(program_transform_name)'; echo as | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

LD_FOR_TARGET=@LD_FOR_TARGET@
USUAL_LD_FOR_TARGET = ` \
  if [ -f $$r/ld/ld-new ] ; then \
    echo $$r/ld/ld-new ; \
  elif [ -f $$r/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=ld ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      echo $(LD); \
    else \
       t='$(program_transform_name)'; echo ld | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

DLLTOOL_FOR_TARGET=@DLLTOOL_FOR_TARGET@
USUAL_DLLTOOL_FOR_TARGET = ` \
  if [ -f $$r/binutils/dlltool ] ; then \
    echo $$r/binutils/dlltool ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      echo $(DLLTOOL); \
    else \
       t='$(program_transform_name)'; echo dlltool | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

WINDRES_FOR_TARGET=@WINDRES_FOR_TARGET@
USUAL_WINDRES_FOR_TARGET = ` \
  if [ -f $$r/binutils/windres ] ; then \
    echo $$r/binutils/windres ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      echo $(WINDRES); \
    else \
       t='$(program_transform_name)'; echo windres | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

AR_FOR_TARGET=@AR_FOR_TARGET@
USUAL_AR_FOR_TARGET = ` \
  if [ -f $$r/binutils/ar ] ; then \
    echo $$r/binutils/ar ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      echo $(AR); \
    else \
       t='$(program_transform_name)'; echo ar | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

RANLIB_FOR_TARGET=@RANLIB_FOR_TARGET@
USUAL_RANLIB_FOR_TARGET = ` \
  if [ -f $$r/binutils/ranlib ] ; then \
    echo $$r/binutils/ranlib ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      if [ x'$(RANLIB)' != x ]; then \
         echo $(RANLIB); \
      else \
         echo ranlib; \
      fi; \
    else \
       t='$(program_transform_name)'; echo ranlib | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

NM_FOR_TARGET=@NM_FOR_TARGET@
USUAL_NM_FOR_TARGET = ` \
  if [ -f $$r/binutils/nm-new ] ; then \
    echo $$r/binutils/nm-new ; \
  elif [ -f $$r/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=nm ; \
  else \
    if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
      echo $(NM); \
    else \
       t='$(program_transform_name)'; echo nm | sed -e 's/x/x/' $$t ; \
    fi; \
  fi`

# The first rule in the file had better be this one.  Don't put any above it.
# This lives here to allow makefile fragments to contain dependencies.
all: all.normal
.PHONY: all

#### host and target specific makefile fragments come in here.
###

# Flags to pass down to all sub-makes.
# Please keep these in alphabetical order.
BASE_FLAGS_TO_PASS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"AR_FOR_TARGET=$(AR_FOR_TARGET)" \
	"AS_FOR_TARGET=$(AS_FOR_TARGET)" \
	"BISON=$(BISON)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"CFLAGS=$(CFLAGS)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"GCJ_FOR_TARGET=$(GCJ_FOR_TARGET)" \
	"CXX_FOR_BUILD=$(CXX_FOR_BUILD)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CXXFLAGS_FOR_TARGET=$(CXXFLAGS_FOR_TARGET)" \
	"CXX_FOR_TARGET=$(CXX_FOR_TARGET)" \
	"DLLTOOL_FOR_TARGET=$(DLLTOOL_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LEX=$(LEX)" \
	"LD_FOR_TARGET=$(LD_FOR_TARGET)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"LIBCXXFLAGS=$(LIBCXXFLAGS)" \
	"LIBCXXFLAGS_FOR_TARGET=$(LIBCXXFLAGS_FOR_TARGET)" \
	"M4=$(M4)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"NM_FOR_TARGET=$(NM_FOR_TARGET)" \
	"RANLIB_FOR_TARGET=$(RANLIB_FOR_TARGET)" \
	"RPATH_ENVVAR=$(RPATH_ENVVAR)" \
	"SHELL=$(SHELL)" \
	"EXPECT=$(EXPECT)" \
	"RUNTEST=$(RUNTEST)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"TARGET_SUBDIR=$(TARGET_SUBDIR)" \
	"WINDRES_FOR_TARGET=$(WINDRES_FOR_TARGET)" \
	"YACC=$(YACC)" \
	"bindir=$(bindir)" \
	"datadir=$(datadir)" \
	"exec_prefix=$(exec_prefix)" \
	"includedir=$(includedir)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"libexecdir=$(libexecdir)" \
	"lispdir=$(lispdir)" \
	"libstdcxx_incdir=$(libstdcxx_incdir)" \
	"libsubdir=$(libsubdir)" \
	"localstatedir=$(localstatedir)" \
	"mandir=$(mandir)" \
	"oldincludedir=$(oldincludedir)" \
	"prefix=$(prefix)" \
	"sbindir=$(sbindir)" \
	"sharedstatedir=$(sharedstatedir)" \
	"sysconfdir=$(sysconfdir)" \
	"tooldir=$(tooldir)" \
	"build_tooldir=$(build_tooldir)" \
	"gxx_include_dir=$(gxx_include_dir)" \
	"gcc_version=$(gcc_version)" \
	"gcc_version_trigger=$(gcc_version_trigger)" \
	"target_alias=$(target_alias)" 

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
# If any variables are added here, they must be added to do-*, below.
EXTRA_HOST_FLAGS = \
	'AR=$(AR)' \
	'AS=$(AS)' \
	'CC=$(CC)' \
	'CXX=$(CXX)' \
	'DLLTOOL=$(DLLTOOL)' \
	'LD=$(LD)' \
	'NM=$(NM)' \
	"`echo 'RANLIB=$(RANLIB)' | sed -e s/.*=$$/XFOO=/`" \
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
# If any variables are added here, they must be added to do-*, below.
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
# The HOST_* variables are a special case, which are used for the gcc
# cross-building scheme.
EXTRA_GCC_FLAGS = \
	'AR=$(AR)' \
	'AS=$(AS)' \
	'CC=$(CC)' \
	'CXX=$(CXX)' \
	'DLLTOOL=$$(DLLTOOL_FOR_TARGET)' \
	'BUILD_CC=$(CC_FOR_BUILD)' \
	'BUILD_PREFIX=$(BUILD_PREFIX)' \
	'BUILD_PREFIX_1=$(BUILD_PREFIX_1)' \
	'NM=$(NM)' \
	"`echo 'RANLIB=$(RANLIB)' | sed -e s/.*=$$/XFOO=/`" \
	'WINDRES=$$(WINDRES_FOR_TARGET)' \
	"GCC_FOR_TARGET=$(GCC_FOR_TARGET)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"`echo 'LANGUAGES=$(LANGUAGES)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'STMP_FIXPROTO=$(STMP_FIXPROTO)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIMITS_H_TEST=$(LIMITS_H_TEST)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIBGCC2_CFLAGS=$(LIBGCC2_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIBGCC2_DEBUG_CFLAGS=$(LIBGCC2_DEBUG_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'LIBGCC2_INCLUDES=$(LIBGCC2_INCLUDES)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'ENQUIRE=$(ENQUIRE)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'STAGE1_CFLAGS=$(STAGE1_CFLAGS)' | sed -e s/.*=$$/XFOO=/`" \
	"`echo 'BOOT_CFLAGS=$(BOOT_CFLAGS)' | sed -e s/.*=$$/XFOO=/`"

GCC_FLAGS_TO_PASS = $(BASE_FLAGS_TO_PASS) $(EXTRA_GCC_FLAGS)

configure-host: @configure_host_modules@
configure-target: @configure_target_modules@

# This is a list of the targets for which we can do a clean-{target}.
CLEAN_MODULES =[+
    FOR host_modules +][+
        IF (not (or (exist? "no_clean") (exist? "with_x"))) +] \
	clean-[+module+][+
        ENDIF no_clean +][+
    ENDFOR host_modules +]

# All of the target modules that can be cleaned
CLEAN_TARGET_MODULES =[+
    FOR target_modules +][+
        IF (not (exist? "no_clean")) +] \
	clean-target-[+module+][+
        ENDIF no_clean +][+
    ENDFOR target_modules +]

# All of the x11 modules that can be cleaned
CLEAN_X11_MODULES = [+ FOR host_modules +][+ IF with_x +]\
	clean-[+module+] [+ ENDIF with_x +][+ ENDFOR host_modules +]

# The target built for a native build.
# This list only includes modules actually being configured and built.
.PHONY: all.normal
all.normal: @all_build_modules@ \
	@all_host_modules@ \
	@all_target_modules@

all-host: @all_host_modules@
all-target: @all_target_modules@

# Do a target for all the subdirectories.  A ``make do-X'' will do a
# ``make X'' in all subdirectories (because, in general, there is a
# dependency (below) of X upon do-X, a ``make X'' will also do this,
# but it may do additional work as well).
# This target ensures that $(BASE_FLAGS_TO_PASS) appears only once,
# because it is so large that it can easily overflow the command line
# length limit on some systems.
[+ FOR recursive_targets +]
.PHONY: do-[+target+]
do-[+target+]:
	@r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	$(SET_LIB_PATH) \
	for i in $(SUBDIRS) -dummy-; do \
	  if [ -f ./$$i/Makefile ]; then \
	    case $$i in \
	    gcc) \
	      for flag in $(EXTRA_GCC_FLAGS); do \
		eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	      done; \
	      ;; \
	    *) \
	      for flag in $(EXTRA_HOST_FLAGS); do \
		eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	      done; \
	      ;; \
	    esac ; \
	    if (cd ./$$i; \
	        $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
			"CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	                "`echo \"RANLIB=$${RANLIB}\" | sed -e 's/.*=$$/XFOO=/'`" \
			"DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
			[+target+]); \
	    then true; else exit 1; fi; \
	  else true; fi; \
	done
	# Break into two pieces
	r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	$(SET_LIB_PATH) \
	for i in $(TARGET_CONFIGDIRS) -dummy-; do \
	  if [ -f $(TARGET_SUBDIR)/$$i/Makefile ]; then \
	    for flag in $(EXTRA_TARGET_FLAGS); do \
		eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	    done; \
	    if (cd $(TARGET_SUBDIR)/$$i; \
	        $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
			"CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	                "`echo \"RANLIB=$${RANLIB}\" | sed -e 's/.*=$$/XFOO=/'`" \
			"DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
			[+target+]); \
	    then true; else exit 1; fi; \
	  else true; fi; \
	done
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
	s=`cd $(srcdir); ${PWD}`; export s; \
	if [ -f dir.info ] ; then \
	  $(INSTALL_DATA) dir.info $(infodir)/dir.info ; \
	else true ; fi

local-clean:
	-rm -f *.a TEMP errs core *.o *~ \#* TAGS *.E *.log

local-distclean:
	-rm -f Makefile config.status config.cache mh-frag mt-frag
	-if [ "$(TARGET_SUBDIR)" != "." ]; then \
	  rm -rf $(TARGET_SUBDIR); \
	else true; fi
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

# This rule is used to clean specific modules.
.PHONY: $(CLEAN_MODULES) $(CLEAN_X11_MODULES) clean-gcc
$(CLEAN_MODULES) $(CLEAN_X11_MODULES) clean-gcc:
	@dir=`echo $@ | sed -e 's/clean-//'`; \
	if [ -f ./$${dir}/Makefile ] ; then \
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd $${dir}; $(MAKE) $(FLAGS_TO_PASS) clean); \
	else \
	  true; \
	fi

.PHONY: $(CLEAN_TARGET_MODULES)
$(CLEAN_TARGET_MODULES):
	@dir=`echo $@ | sed -e 's/clean-target-//'`; \
	rm -f $(TARGET_SUBDIR)/$${dir}/multilib.out $(TARGET_SUBDIR)/$${dir}/tmpmulti.out; \
	if [ -f $(TARGET_SUBDIR)/$${dir}/Makefile ] ; then \
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd $(TARGET_SUBDIR)/$${dir}; $(MAKE) $(TARGET_FLAGS_TO_PASS) clean); \
	else \
	  true; \
	fi

clean-target: $(CLEAN_TARGET_MODULES) clean-target-libgcc
clean-target-libgcc:
	test ! -d gcc/libgcc || \
	(cd gcc/libgcc && find . -type d -print) | \
	while read d; do rm -f gcc/$$d/libgcc.a || : ; done
	-rm -rf gcc/libgcc

# Check target.

.PHONY: check do-check
check:
	$(MAKE) do-check NOTPARALLEL=parallel-ok

# Only include modules actually being configured and built.
do-check: @check_host_modules@ \
	@check_target_modules@

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
install: installdirs @install_host_modules@ @install_target_modules@

install-target: @install_target_modules@

uninstall:
	@echo "the uninstall target is not supported in this tree"

.PHONY: install.all
install.all: install-no-fixedincludes
	@if [ -f ./gcc/Makefile ] ; then \
		r=`${PWD}` ; export r ; \
		$(SET_LIB_PATH) \
		(cd ./gcc; \
		$(MAKE) $(FLAGS_TO_PASS) install-headers) ; \
	else \
		true ; \
	fi

# install-no-fixedincludes is used because Cygnus can not distribute
# the fixed header files.
.PHONY: install-no-fixedincludes
install-no-fixedincludes: installdirs @install_host_modules_nogcc@ \
	@install_target_modules@ gcc-no-fixedincludes

### other supporting targets

MAKEDIRS= \
	$(DESTDIR)$(prefix) \
	$(DESTDIR)$(exec_prefix)
.PHONY: installdirs
installdirs: mkinstalldirs
	$(SHELL) $(srcdir)/mkinstalldirs $(MAKEDIRS)

dir.info: do-install-info
	if [ -f $(srcdir)/texinfo/gen-info-dir ] ; then \
	  $(srcdir)/texinfo/gen-info-dir $(infodir) $(srcdir)/texinfo/dir.info-template > dir.info.new ; \
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
configure-build-[+module+]: $(BUILD_SUBDIR)/[+module+]/Makefile
@build_prefix@$(BUILD_SUBDIR)/[+module+]/Makefile: config.status
	@[ -d $(BUILD_SUBDIR)/[+module+] ] || mkdir $(BUILD_SUBDIR)/[+module+];\
	    r=`${PWD}`; export r; \
	    s=`cd $(srcdir); ${PWD}`; export s; \
	    AR="$(AR_FOR_BUILD)"; export AR; \
	    AS="$(AS_FOR_BUILD)"; export AS; \
	    CC="$(CC_FOR_BUILD)"; export CC; \
	    CFLAGS="$(CFLAGS_FOR_BUILD)"; export CFLAGS; \
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
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  (cd $(BUILD_SUBDIR)/[+module+] && $(MAKE) all)
[+ ENDFOR build_modules +]

# --------------------------------------
# Modules which run on the host machine
# --------------------------------------
[+ FOR host_modules +]
.PHONY: configure-[+module+] maybe-configure-[+module+]
maybe-configure-[+module+]:
configure-[+module+]: [+module+]/Makefile

[+module+]/Makefile: config.status
	@[ -d [+module+] ] || mkdir [+module+]; \
	r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	CC="$(CC)"; export CC; \
	CFLAGS="$(CFLAGS)"; export CFLAGS; \
	CXX="$(CXX)"; export CXX; \
	CXXFLAGS="$(CXXFLAGS)"; export CXXFLAGS; \
	if [ z$(build_canonical) !=  z$(host_canoncial) ] ; then \
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
	fi; \
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
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd [+module+]; $(MAKE) $(FLAGS_TO_PASS)[+ 
	    IF with_x 
	      +] $(X11_FLAGS_TO_PASS)[+ 
	    ENDIF with_x +] all)

[+ IF no_check +]
.PHONY: check-[+module+]
check-[+module+]:
[+ ELIF no_check_cross +]
.PHONY: check-[+module+]
# This module is only tested in a native toolchain.
check-[+module+]:
	@if [ '$(host_canonical)' = '$(target_canonical)' ] ; then \
	    r=`${PWD}`; export r; \
	    s=`cd $(srcdir); ${PWD}`; export s; \
	    $(SET_LIB_PATH) \
	    (cd [+module+]; $(MAKE) $(FLAGS_TO_PASS)[+ 
	      IF with_x 
	        +] $(X11_FLAGS_TO_PASS)[+ 
	      ENDIF with_x +] check); \
	fi
[+ ELSE check +]
.PHONY: check-[+module+]
check-[+module+]:
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd [+module+]; $(MAKE) $(FLAGS_TO_PASS)[+ 
	    IF with_x 
	      +] $(X11_FLAGS_TO_PASS)[+ 
	    ENDIF with_x +] check)
[+ ENDIF no_check +]

[+ IF no_install +]
.PHONY: install-[+module+] maybe-install-[+module+]
maybe-install-[+module+]:
install-[+module+]:
[+ ELSE install +]
.PHONY: install-[+module+] maybe-install-[+module+]
maybe-install-[+module+]:
install-[+module+]: installdirs
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd [+module+]; $(MAKE) $(FLAGS_TO_PASS)[+ 
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
configure-target-[+module+]: $(TARGET_SUBDIR)/[+module+]/Makefile

# Don't manually override CC_FOR_TARGET at make time; get it set right
# at configure time.  Otherwise multilibs may be wrong.
$(TARGET_SUBDIR)/[+module+]/multilib.out: maybe-all-gcc
	@[ -d $(TARGET_SUBDIR)/[+module+] ] || mkdir $(TARGET_SUBDIR)/[+module+];\
	r=`${PWD}`; export r; \
	echo "Configuring multilibs for [+module+]"; \
	$(CC_FOR_TARGET) --print-multi-lib > $(TARGET_SUBDIR)/[+module+]/multilib.out 2> /dev/null

$(TARGET_SUBDIR)/[+module+]/Makefile: config.status $(TARGET_SUBDIR)/[+module+]/multilib.out
	@[ -d $(TARGET_SUBDIR)/[+module+] ] || mkdir $(TARGET_SUBDIR)/[+module+];\
	    r=`${PWD}`; export r; \
	    s=`cd $(srcdir); ${PWD}`; export s; \
	    $(SET_LIB_PATH) \
	    AR="$(AR_FOR_TARGET)"; export AR; \
	    AS="$(AS_FOR_TARGET)"; export AS; \
	    CC="$(CC_FOR_TARGET)"; export CC; \
	    CFLAGS="$(CFLAGS_FOR_TARGET)"; export CFLAGS; \[+ 
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
	    esac; \
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
	    else \
	      srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	      libsrcdir="$$s/[+module+]"; \
	    fi; \
	    rm -f no-such-file || : ; \
	    CONFIG_SITE=no-such-file $(SHELL) $${libsrcdir}/configure \
	      $(TARGET_CONFIGARGS) $${srcdiroption} \
	      --with-target-subdir="$(TARGET_SUBDIR)" \
	      || exit 1

.PHONY: all-target-[+module+] maybe-all-target-[+module+]
maybe-all-target-[+module+]:
all-target-[+module+]: configure-target-[+module+]
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd $(TARGET_SUBDIR)/[+module+]; \
	    $(MAKE) $(TARGET_FLAGS_TO_PASS) [+
	       IF raw_cxx 
	         +] 'CXX=$$(RAW_CXX_FOR_TARGET)' 'CXX_FOR_TARGET=$$(RAW_CXX_FOR_TARGET)' [+ 
	       ENDIF raw_cxx 
	    +] all)
[+ IF no_check +]
# Dummy target for uncheckable module.
.PHONY: check-target-[+module+]
check-target-[+module+]:
[+ ELSE check +]
.PHONY: check-target-[+module+]
check-target-[+module+]:
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd $(TARGET_SUBDIR)/[+module+]; \
	    $(MAKE) $(TARGET_FLAGS_TO_PASS) [+
	       IF raw_cxx 
	         +] 'CXX=$$(RAW_CXX_FOR_TARGET)' 'CXX_FOR_TARGET=$$(RAW_CXX_FOR_TARGET)' [+ 
	       ENDIF raw_cxx 
	    +] check)
[+ ENDIF no_check +]
[+ IF no_install +]
.PHONY: install-target-[+module+] maybe-install-target-[+module+]
maybe-install-target-[+module+]:
# Dummy target for uninstallable.
install-target-[+module+]:
[+ ELSE install +]
.PHONY: install-target-[+module+] maybe-install-target-[+module+]
maybe-install-target-[+module+]:
install-target-[+module+]: installdirs
	@r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd $(TARGET_SUBDIR)/[+module+]; \
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
.PHONY: configure-gcc maybe-configure-gcc
maybe-configure-gcc:
configure-gcc: gcc/Makefile

gcc/Makefile: config.status
	@[ -d gcc ] || mkdir gcc; \
	r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	CC="$(CC)"; export CC; \
	CFLAGS="$(CFLAGS)"; export CFLAGS; \
	CXX="$(CXX)"; export CXX; \
	CXXFLAGS="$(CXXFLAGS)"; export CXXFLAGS; \
	if [ z$(build_canonical) !=  z$(host_canoncial) ] ; then \
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
	fi; \
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
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc; $(MAKE) $(GCC_FLAGS_TO_PASS) quickstrap); \
	else \
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc; $(MAKE) $(GCC_FLAGS_TO_PASS) all); \
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
.PHONY: bootstrap bootstrap-lean bootstrap2 bootstrap2-lean bootstrap3 bootstrap3-lean bootstrap4 bootstrap4-lean bubblestrap quickstrap cleanstrap restrap
bootstrap bootstrap-lean bootstrap2 bootstrap2-lean bootstrap3 bootstrap3-lean bootstrap4 bootstrap4-lean bubblestrap quickstrap cleanstrap restrap: all-bootstrap configure-gcc
	@r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	$(SET_LIB_PATH) \
	echo "Bootstrapping the compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) $@
	@r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	case "$@" in \
	  *bootstrap4-lean ) \
			msg="Comparing stage3 and stage4 of the compiler"; \
	  		compare=compare3-lean ;; \
	  *bootstrap4 ) msg="Comparing stage3 and stage4 of the compiler"; \
	  		compare=compare3 ;; \
	  *-lean )	msg="Comparing stage2 and stage3 of the compiler"; \
	  		compare=compare-lean ;; \
	  * )		msg="Comparing stage2 and stage3 of the compiler"; \
	  		compare=compare ;; \
	esac; \
	$(SET_LIB_PATH) \
	echo "$$msg"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) $$compare
	@r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS) all

.PHONY: cross
cross: all-texinfo all-bison all-byacc all-binutils all-gas all-ld
	@r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}`; export s; \
	$(SET_LIB_PATH) \
	echo "Building the C and C++ compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) LANGUAGES="c c++"
	@r=`${PWD}`; export r; \
	s=`cd $(srcdir); ${PWD}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS) \
	  LANGUAGES="c c++" all

.PHONY: check-gcc
check-gcc:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc; $(MAKE) $(GCC_FLAGS_TO_PASS) check); \
	else \
	  true; \
	fi

.PHONY: check-gcc-c++
check-gcc-c++:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc; $(MAKE) $(GCC_FLAGS_TO_PASS) check-c++); \
	else \
	  true; \
	fi

.PHONY: check-c++
check-c++: check-target-libstdc++-v3 check-gcc-c++

.PHONY: install-gcc maybe-install-gcc
maybe-install-gcc:
install-gcc:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}`; export s; \
	  $(SET_LIB_PATH) \
	  (cd gcc; $(MAKE) $(GCC_FLAGS_TO_PASS) install); \
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
	  r=`${PWD}`; export r; \
	  s=`cd $(srcdir); ${PWD}` ; export s; \
	  $(SET_LIB_PATH) \
	  (cd ./gcc; \
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
configure-gcc: maybe-configure-binutils maybe-configure-gas maybe-configure-ld maybe-configure-bison maybe-configure-flex
all-gcc: maybe-all-libiberty maybe-all-bison maybe-all-byacc maybe-all-binutils maybe-all-gas maybe-all-ld maybe-all-zlib
# This is a slightly kludgy method of getting dependencies on 
# all-build-libiberty correct; it would be better to build it every time.
all-gcc: maybe-all-build-libiberty
all-bootstrap: maybe-all-libiberty maybe-all-texinfo maybe-all-bison maybe-all-byacc maybe-all-binutils maybe-all-gas maybe-all-ld maybe-all-zlib

# Host modules specific to gdb.
# GDB needs to know that the simulator is being built.
configure-gdb: maybe-configure-tcl maybe-configure-tk maybe-configure-sim
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
all-sid: maybe-all-tcl maybe-all-tk
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
all-grep: maybe-all-libiberty
all-gzip: maybe-all-libiberty
all-hello: maybe-all-libiberty
all-m4: maybe-all-libiberty maybe-all-texinfo
all-make: maybe-all-libiberty
all-patch: maybe-all-libiberty
all-prms: maybe-all-libiberty
all-recode: maybe-all-libiberty
all-sed: maybe-all-libiberty
all-send-pr: maybe-all-prms
all-snavigator: maybe-all-tcl maybe-all-tk maybe-all-itcl maybe-all-tix maybe-all-db maybe-all-grep maybe-all-libgui
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
configure-target-libiberty: $(ALL_GCC_C)
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

# Rebuilding Makefile.in, using autogen.
$(srcdir)/Makefile.in: # $(srcdir)/Makefile.tpl $(srcdir)/Makefile.def
	cd $(srcdir) && autogen Makefile.def

# with the gnu make, this is done automatically.

host_makefile_frag=@host_makefile_frag@
target_makefile_frag=@target_makefile_frag@

Makefile: Makefile.in configure.in $(host_makefile_frag) $(target_makefile_frag) $(gcc_version_trigger)
	$(SHELL) ./config.status

#

.NOEXPORT:
MAKEOVERRIDES=

# end of Makefile.in
