[+ AutoGen5 template -*- Mode: Makefile -*-
in
+]

# Makefile.in is generated from Makefile.tpl by 'autogen Makefile.def'.
#
# Makefile for directory with subdirs to build.
#   Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
#   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation
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
LN = @LN@
LN_S = @LN_S@

# -------------------------------------------------
# Miscellaneous non-standard autoconf-set variables
# -------------------------------------------------

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

# This is the list of variables to export in the environment when
# configuring any subdirectory.  It must also be exported whenever
# recursing into a build directory in case that directory's Makefile
# re-runs configure.
BASE_EXPORTS = \
	FLEX="$(FLEX)"; export FLEX; \
	LEX="$(LEX)"; export LEX; \
	BISON="$(BISON)"; export BISON; \
	YACC="$(YACC)"; export YACC; \
	M4="$(M4)"; export M4; \
	MAKEINFO="$(MAKEINFO)"; export MAKEINFO;

# This is the list of variables to export in the environment when
# configuring subdirectories for the build system.
BUILD_EXPORTS = \
	$(BASE_EXPORTS) \
	AR="$(AR_FOR_BUILD)"; export AR; \
	AS="$(AS_FOR_BUILD)"; export AS; \
	CC="$(CC_FOR_BUILD)"; export CC; \
	CFLAGS="$(CFLAGS_FOR_BUILD)"; export CFLAGS; \
	CONFIG_SHELL="$(SHELL)"; export CONFIG_SHELL; \
	CXX="$(CXX_FOR_BUILD)"; export CXX; \
	CXXFLAGS="$(CXXFLAGS_FOR_BUILD)"; export CXXFLAGS; \
	GCJ="$(GCJ_FOR_BUILD)"; export GCJ; \
	GFORTRAN="$(GFORTRAN_FOR_BUILD)"; export GFORTRAN; \
	DLLTOOL="$(DLLTOOL_FOR_BUILD)"; export DLLTOOL; \
	LD="$(LD_FOR_BUILD)"; export LD; \
	LDFLAGS="$(LDFLAGS_FOR_BUILD)"; export LDFLAGS; \
	NM="$(NM_FOR_BUILD)"; export NM; \
	RANLIB="$(RANLIB_FOR_BUILD)"; export RANLIB; \
	WINDRES="$(WINDRES_FOR_BUILD)"; export WINDRES;

# This is the list of directories to built for the host system.
SUBDIRS = @configdirs@
# This is set by the configure script to the arguments to use when configuring
# directories built for the host system.
HOST_CONFIGARGS = @host_configargs@
# Host programs are put under this directory, which is . except if building
# with srcdir=..
HOST_SUBDIR = @host_subdir@
# This is the list of variables to export in the environment when
# configuring subdirectories for the host system.
HOST_EXPORTS = \
	$(BASE_EXPORTS) \
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
	LDFLAGS="$(LDFLAGS)"; export LDFLAGS; \
	NM="$(NM)"; export NM; \
	RANLIB="$(RANLIB)"; export RANLIB; \
	WINDRES="$(WINDRES)"; export WINDRES; \
	OBJCOPY="$(OBJCOPY)"; export OBJCOPY; \
	OBJDUMP="$(OBJDUMP)"; export OBJDUMP; \
	TOPLEVEL_CONFIGURE_ARGUMENTS="$(TOPLEVEL_CONFIGURE_ARGUMENTS)"; export TOPLEVEL_CONFIGURE_ARGUMENTS; \
	GMPLIBS="$(HOST_GMPLIBS)"; export GMPLIBS; \
	GMPINC="$(HOST_GMPINC)"; export GMPINC; \
	SET_GCC_LIB_PATH_CMD="@SET_GCC_LIB_PATH@"; export SET_GCC_LIB_PATH_CMD; \
	@SET_GCC_LIB_PATH@

# Similar, for later GCC stages.
STAGE_HOST_EXPORTS = \
	$(HOST_EXPORTS) \
	CC="$(STAGE_CC_WRAPPER) $$r/$(HOST_SUBDIR)/prev-gcc/xgcc$(exeext) \
	  -B$$r/$(HOST_SUBDIR)/prev-gcc/ -B$(build_tooldir)/bin/"; export CC; \
	CC_FOR_BUILD="$(STAGE_CC_WRAPPER) \
	  $$r/$(HOST_SUBDIR)/prev-gcc/xgcc$(exeext) \
	  -B$$r/$(HOST_SUBDIR)/prev-gcc/ \
	  -B$(build_tooldir)/bin/"; export CC_FOR_BUILD;

# This is set by the configure script to the list of directories which
# should be built using the target tools.
TARGET_CONFIGDIRS = @target_configdirs@
# Target libraries are put under this directory:
TARGET_SUBDIR = @target_subdir@
# This is set by the configure script to the arguments to use when configuring
# directories built for the target.
TARGET_CONFIGARGS = @target_configargs@
# This is the list of variables to export in the environment when
# configuring subdirectories for the host system.
BASE_TARGET_EXPORTS = \
	$(BASE_EXPORTS) \
	AR="$(AR_FOR_TARGET)"; export AR; \
	AS="$(AS_FOR_TARGET)"; export AS; \
	CC="$(CC_FOR_TARGET)"; export CC; \
	CFLAGS="$(CFLAGS_FOR_TARGET)"; export CFLAGS; \
	CONFIG_SHELL="$(SHELL)"; export CONFIG_SHELL; \
	CPPFLAGS="$(CPPFLAGS_FOR_TARGET)"; export CPPFLAGS; \
	CXXFLAGS="$(CXXFLAGS_FOR_TARGET)"; export CXXFLAGS; \
	GCJ="$(GCJ_FOR_TARGET)"; export GCJ; \
	GFORTRAN="$(GFORTRAN_FOR_TARGET)"; export GFORTRAN; \
	DLLTOOL="$(DLLTOOL_FOR_TARGET)"; export DLLTOOL; \
	LD="$(LD_FOR_TARGET)"; export LD; \
	LDFLAGS="$(LDFLAGS_FOR_TARGET)"; export LDFLAGS; \
	NM="$(NM_FOR_TARGET)"; export NM; \
	RANLIB="$(RANLIB_FOR_TARGET)"; export RANLIB; \
	WINDRES="$(WINDRES_FOR_TARGET)"; export WINDRES; \
	SET_GCC_LIB_PATH_CMD="@SET_GCC_LIB_PATH@"; export SET_GCC_LIB_PATH_CMD; \
	@SET_GCC_LIB_PATH@

RAW_CXX_TARGET_EXPORTS = \
	$(BASE_TARGET_EXPORTS) \
	CXX_FOR_TARGET="$(RAW_CXX_FOR_TARGET)"; export CXX_FOR_TARGET; \
	CXX="$(RAW_CXX_FOR_TARGET)"; export CXX;

NORMAL_TARGET_EXPORTS = \
	$(BASE_TARGET_EXPORTS) \
	CXX="$(CXX_FOR_TARGET)"; export CXX;

# Where to find GMP
HOST_GMPLIBS = @gmplibs@
HOST_GMPINC = @gmpinc@

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

# Flags to pass to stage2 and later makes.  They are defined
# here so that they can be overridden by Makefile fragments.
BOOT_CFLAGS= -g -O2

CONFIGURED_BISON = @CONFIGURED_BISON@
BISON = `if [ -f $$r/$(BUILD_SUBDIR)/bison/tests/bison ] ; then \
	    echo $$r/$(BUILD_SUBDIR)/bison/tests/bison ; \
	 else \
	    echo ${CONFIGURED_BISON} ; \
	 fi`

CONFIGURED_YACC = @CONFIGURED_YACC@
YACC = `if [ -f $$r/$(BUILD_SUBDIR)/bison/tests/bison ] ; then \
	    echo $$r/$(BUILD_SUBDIR)/bison/tests/bison -y ; \
	elif [ -f $$r/$(BUILD_SUBDIR)/byacc/byacc ] ; then \
	    echo $$r/$(BUILD_SUBDIR)/byacc/byacc ; \
	else \
	    echo ${CONFIGURED_YACC} ; \
	fi`

CONFIGURED_FLEX = @CONFIGURED_FLEX@
FLEX = `if [ -f $$r/$(BUILD_SUBDIR)/flex/flex ] ; \
	then echo $$r/$(BUILD_SUBDIR)/flex/flex ; \
	else echo ${CONFIGURED_FLEX} ; fi`

CONFIGURED_LEX = @CONFIGURED_LEX@
LEX = `if [ -f $$r/$(BUILD_SUBDIR)/flex/flex ] ; \
	then echo $$r/$(BUILD_SUBDIR)/flex/flex ; \
	else echo ${CONFIGURED_LEX} ; fi`

CONFIGURED_M4 = @CONFIGURED_M4@
M4 = `if [ -f $$r/$(BUILD_SUBDIR)/m4/m4 ] ; \
	then echo $$r/$(BUILD_SUBDIR)/m4/m4 ; \
	else echo ${CONFIGURED_M4} ; fi`

# For an installed makeinfo, we require it to be from texinfo 4.2 or
# higher, else we use the "missing" dummy.  We also pass the subdirectory
# makeinfo even if only the Makefile is there, because Texinfo builds its
# manual when made, and it requires its own version.
CONFIGURED_MAKEINFO = @CONFIGURED_MAKEINFO@
MAKEINFO = `if [ -f $$r/$(BUILD_SUBDIR)/texinfo/makeinfo/Makefile ] ; \
	then echo $$r/$(BUILD_SUBDIR)/texinfo/makeinfo/makeinfo ; \
	else if (${CONFIGURED_MAKEINFO} --version \
	  | egrep 'texinfo[^0-9]*([1-3][0-9]|4\.[2-9]|[5-9])') >/dev/null 2>&1; \
        then echo ${CONFIGURED_MAKEINFO}; else echo $$s/missing makeinfo; fi; fi`

# This just becomes part of the MAKEINFO definition passed down to
# sub-makes.  It lets flags be given on the command line while still
# using the makeinfo from the object tree.
# (Default to avoid splitting info files by setting the threshold high.)
MAKEINFOFLAGS = --split-size=5000000

# FIXME: expect may become a build tool?
EXPECT = `if [ -f $$r/$(HOST_SUBDIR)/expect/expect ] ; \
	then echo $$r/$(HOST_SUBDIR)/expect/expect ; \
	else echo expect ; fi`

RUNTEST = `if [ -f $$s/dejagnu/runtest ] ; \
	then echo $$s/dejagnu/runtest ; \
	else echo runtest ; fi`

# ---------------------------------------------
# Programs producing files for the HOST machine
# ---------------------------------------------

# This is the list of directories that may be needed in RPATH_ENVVAR
# so that programs built for the host machine work.
HOST_LIB_PATH = $$r/$(HOST_SUBDIR)/bfd:$$r/$(HOST_SUBDIR)/opcodes

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
TARGET_LIB_PATH = $$r/$(TARGET_SUBDIR)/libstdc++-v3/src/.libs:$$r/$(TARGET_SUBDIR)/libmudflap/.libs

FLAGS_FOR_TARGET = @FLAGS_FOR_TARGET@

AR_FOR_TARGET=@AR_FOR_TARGET@
CONFIGURED_AR_FOR_TARGET=@CONFIGURED_AR_FOR_TARGET@
USUAL_AR_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/binutils/ar ] ; then \
    echo $$r/$(HOST_SUBDIR)/binutils/ar ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(AR); \
    else \
      echo $(CONFIGURED_AR_FOR_TARGET) ; \
    fi; \
  fi`

AS_FOR_TARGET=@AS_FOR_TARGET@
CONFIGURED_AS_FOR_TARGET=@CONFIGURED_AS_FOR_TARGET@
USUAL_AS_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/gas/as-new ] ; then \
    echo $$r/$(HOST_SUBDIR)/gas/as-new ; \
  elif [ -f $$r/$(HOST_SUBDIR)/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=as ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(AS); \
    else \
      echo $(CONFIGURED_AS_FOR_TARGET) ; \
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
USUAL_GCC_FOR_TARGET = $(STAGE_CC_WRAPPER) \
  $$r/$(HOST_SUBDIR)/gcc/xgcc -B$$r/$(HOST_SUBDIR)/gcc/ $(FLAGS_FOR_TARGET)
LIBCFLAGS_FOR_TARGET = $(CFLAGS_FOR_TARGET)

CXX_FOR_TARGET = @CXX_FOR_TARGET@
RAW_CXX_FOR_TARGET = @RAW_CXX_FOR_TARGET@
CXX_FOR_TARGET_FOR_RECURSIVE_MAKE = @CXX_FOR_TARGET_FOR_RECURSIVE_MAKE@
RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE = @RAW_CXX_FOR_TARGET_FOR_RECURSIVE_MAKE@
CXXFLAGS_FOR_TARGET = $(CXXFLAGS)
LIBCXXFLAGS_FOR_TARGET = $(CXXFLAGS_FOR_TARGET) -fno-implicit-templates

DLLTOOL_FOR_TARGET=@DLLTOOL_FOR_TARGET@
CONFIGURED_DLLTOOL_FOR_TARGET=@CONFIGURED_DLLTOOL_FOR_TARGET@
USUAL_DLLTOOL_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/binutils/dlltool ] ; then \
    echo $$r/$(HOST_SUBDIR)/binutils/dlltool ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(DLLTOOL); \
    else \
      echo $(CONFIGURED_DLLTOOL_FOR_TARGET) ; \
    fi; \
  fi`

GCJ_FOR_TARGET = @GCJ_FOR_TARGET@
GFORTRAN_FOR_TARGET = @GFORTRAN_FOR_TARGET@

LD_FOR_TARGET=@LD_FOR_TARGET@
CONFIGURED_LD_FOR_TARGET=@CONFIGURED_LD_FOR_TARGET@
USUAL_LD_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/ld/ld-new ] ; then \
    echo $$r/$(HOST_SUBDIR)/ld/ld-new ; \
  elif [ -f $$r/$(HOST_SUBDIR)/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=ld ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(LD); \
    else \
      echo $(CONFIGURED_LD_FOR_TARGET) ; \
    fi; \
  fi`

LDFLAGS_FOR_TARGET = 

NM_FOR_TARGET=@NM_FOR_TARGET@
CONFIGURED_NM_FOR_TARGET=@CONFIGURED_NM_FOR_TARGET@
USUAL_NM_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/binutils/nm-new ] ; then \
    echo $$r/$(HOST_SUBDIR)/binutils/nm-new ; \
  elif [ -f $$r/$(HOST_SUBDIR)/gcc/xgcc ]; then \
    $(CC_FOR_TARGET) -print-prog-name=nm ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(NM); \
    else \
      echo $(CONFIGURED_NM_FOR_TARGET) ; \
    fi; \
  fi`

RANLIB_FOR_TARGET=@RANLIB_FOR_TARGET@
CONFIGURED_RANLIB_FOR_TARGET=@CONFIGURED_RANLIB_FOR_TARGET@
USUAL_RANLIB_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/binutils/ranlib ] ; then \
    echo $$r/$(HOST_SUBDIR)/binutils/ranlib ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      if [ x'$(RANLIB)' != x ]; then \
         echo $(RANLIB); \
      else \
         echo ranlib; \
      fi; \
    else \
      echo $(CONFIGURED_RANLIB_FOR_TARGET) ; \
    fi; \
  fi`

WINDRES_FOR_TARGET=@WINDRES_FOR_TARGET@
CONFIGURED_WINDRES_FOR_TARGET=@CONFIGURED_WINDRES_FOR_TARGET@
USUAL_WINDRES_FOR_TARGET = ` \
  if [ -f $$r/$(HOST_SUBDIR)/binutils/windres ] ; then \
    echo $$r/$(HOST_SUBDIR)/binutils/windres ; \
  else \
    if [ '$(host)' = '$(target)' ] ; then \
      echo $(WINDRES); \
    else \
      echo $(CONFIGURED_WINDRES_FOR_TARGET) ; \
    fi; \
  fi`

PICFLAG_FOR_TARGET = 

# ------------------------------------
# Miscellaneous targets and flag lists
# ------------------------------------

# The first rule in the file had better be this one.  Don't put any above it.
# This lives here to allow makefile fragments to contain dependencies.
@default_target@:

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

RECURSE_FLAGS_TO_PASS = $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS)

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
	"`echo 'LANGUAGES=$(LANGUAGES)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'STMP_FIXPROTO=$(STMP_FIXPROTO)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'LIMITS_H_TEST=$(LIMITS_H_TEST)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'LIBGCC2_CFLAGS=$(LIBGCC2_CFLAGS)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'LIBGCC2_DEBUG_CFLAGS=$(LIBGCC2_DEBUG_CFLAGS)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'LIBGCC2_INCLUDES=$(LIBGCC2_INCLUDES)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'STAGE1_CFLAGS=$(STAGE1_CFLAGS)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'BOOT_CFLAGS=$(BOOT_CFLAGS)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`" \
	"`echo 'BOOT_ADAFLAGS=$(BOOT_ADAFLAGS)' | sed -e s'/[^=][^=]*=$$/XFOO=/'`"

GCC_FLAGS_TO_PASS = $(BASE_FLAGS_TO_PASS) $(EXTRA_HOST_FLAGS) $(EXTRA_GCC_FLAGS)

.PHONY: configure-host
configure-host: [+
  FOR host_modules +] \
    maybe-configure-[+module+][+
  ENDFOR host_modules +]
.PHONY: configure-target
configure-target: [+
  FOR target_modules +] \
    maybe-configure-target-[+module+][+
  ENDFOR target_modules +]

# The target built for a native non-bootstrap build.
.PHONY: all
all: unstage all-host all-target stage

.PHONY: all-build
all-build: [+
  FOR build_modules +] \
    maybe-all-build-[+module+][+
  ENDFOR build_modules +]
.PHONY: all-host
all-host: [+
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
do-[+make_target+]: unstage [+make_target+]-host [+make_target+]-target stage

.PHONY: [+make_target+]-host
[+make_target+]-host: [+
  FOR host_modules +] \
    maybe-[+make_target+]-[+module+][+
  ENDFOR host_modules +]

.PHONY: [+make_target+]-target
[+make_target+]-target: [+
  FOR target_modules +] \
    maybe-[+make_target+]-target-[+module+][+
  ENDFOR target_modules +]
[+ ENDFOR recursive_targets +]

# Here are the targets which correspond to the do-X targets.

.PHONY: info installcheck dvi html install-info
.PHONY: clean distclean mostlyclean maintainer-clean realclean
.PHONY: local-clean local-distclean local-maintainer-clean
info: do-info
installcheck: do-installcheck
dvi: do-dvi
html: do-html

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
	-if [ "$(HOST_SUBDIR)" != "." ]; then \
	  rm -rf $(HOST_SUBDIR); \
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

# Extra dependency for clean-target, owing to the mixed nature of gcc
clean-target: clean-target-libgcc
clean-target-libgcc:
	test ! -d gcc/libgcc || \
	(cd gcc/libgcc && find . -type d -print) | \
	while read d; do rm -f gcc/$$d/libgcc.a || : ; done
	-rm -rf gcc/libgcc
	-rm -f gcc/stmp-dirs

# Check target.

.PHONY: check do-check
check: do-check

# Only include modules actually being configured and built.
do-check: unstage [+
  FOR host_modules +] \
    maybe-check-[+module+][+
  ENDFOR host_modules +][+
  FOR target_modules +] \
    maybe-check-target-[+module+][+
  ENDFOR target_modules +] stage

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
  FOR host_modules +][+ IF (not (= (get "module") "gcc")) +] \
    maybe-install-[+module+][+ ENDIF +][+
  ENDFOR host_modules +]

.PHONY: install-host
install-host: [+
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
		$(HOST_EXPORTS) \
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
@if build-[+module+]
maybe-configure-build-[+module+]: configure-build-[+module+]
configure-build-[+module+]:
	@test ! -f $(BUILD_SUBDIR)/[+module+]/Makefile || exit 0; \
	$(SHELL) $(srcdir)/mkinstalldirs $(BUILD_SUBDIR)/[+module+] ; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(BUILD_EXPORTS) \
	echo Configuring in $(BUILD_SUBDIR)/[+module+]; \
	cd "$(BUILD_SUBDIR)/[+module+]" || exit 1; \
	case $(srcdir) in \
	  /* | [A-Za-z]:[\\/]*) topdir=$(srcdir) ;; \
	  *) topdir=`echo $(BUILD_SUBDIR)/[+module+]/ | \
		sed -e 's,\./,,g' -e 's,[^/]*/,../,g' `$(srcdir) ;; \
	esac; \
	srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	libsrcdir="$$s/[+module+]"; \
	rm -f no-such-file || : ; \
	CONFIG_SITE=no-such-file $(SHELL) $${libsrcdir}/configure \
	  $(BUILD_CONFIGARGS) $${srcdiroption} \
	  --with-build-subdir="$(BUILD_SUBDIR)" [+extra_configure_flags+] \
	  || exit 1
@endif build-[+module+]

.PHONY: all-build-[+module+] maybe-all-build-[+module+]
maybe-all-build-[+module+]:
@if build-[+module+]
TARGET-build-[+module+]=[+ IF target +][+target+][+ ELSE +]all[+ ENDIF target +]
maybe-all-build-[+module+]: all-build-[+module+]
all-build-[+module+]: configure-build-[+module+]
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(BUILD_EXPORTS) \
	(cd $(BUILD_SUBDIR)/[+module+] && \
	  $(MAKE) [+extra_make_flags+] $(TARGET-build-[+module+]))
@endif build-[+module+]
[+ ENDFOR build_modules +]

# --------------------------------------
# Modules which run on the host machine
# --------------------------------------
[+ FOR host_modules +]
.PHONY: configure-[+module+] maybe-configure-[+module+]
maybe-configure-[+module+]:
@if [+module+]
maybe-configure-[+module+]: configure-[+module+]
configure-[+module+]:
	@[+ IF bootstrap +]test -f stage_last && exit 0; \
	[+ ENDIF bootstrap +]test ! -f $(HOST_SUBDIR)/[+module+]/Makefile || exit 0; \
	$(SHELL) $(srcdir)/mkinstalldirs $(HOST_SUBDIR)/[+module+] ; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(HOST_EXPORTS) \
	echo Configuring in [+module+]; \
	cd $(HOST_SUBDIR)/[+module+] || exit 1; \
	case $(srcdir) in \
	  /* | [A-Za-z]:[\\/]*) topdir=$(srcdir) ;; \
	  *) topdir=`echo $(HOST_SUBDIR)/[+module+]/ | \
		sed -e 's,\./,,g' -e 's,[^/]*/,../,g' `$(srcdir) ;; \
	esac; \
	srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	libsrcdir="$$s/[+module+]"; \
	$(SHELL) $${libsrcdir}/configure \
	  $(HOST_CONFIGARGS) $${srcdiroption} [+extra_configure_flags+] \
	  || exit 1
@endif [+module+]

.PHONY: all-[+module+] maybe-all-[+module+]
maybe-all-[+module+]:
@if [+module+]
TARGET-[+module+]=[+ IF target +][+target+][+ ELSE +]all[+ ENDIF target +]
maybe-all-[+module+]: all-[+module+]
all-[+module+]: configure-[+module+]
	@[+ IF bootstrap +]test -f stage_last && exit 0; \
	[+ ENDIF bootstrap +]r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	(cd $(HOST_SUBDIR)/[+module+] && \
	  $(MAKE) $(FLAGS_TO_PASS) [+extra_make_flags+] $(TARGET-[+module+]))
@endif [+module+]

.PHONY: check-[+module+] maybe-check-[+module+]
maybe-check-[+module+]:
@if [+module+]
maybe-check-[+module+]: check-[+module+]
[+ IF no_check +]
check-[+module+]:
[+ ELIF no_check_cross +]
# This module is only tested in a native toolchain.
check-[+module+]:
	@if [ '$(host)' = '$(target)' ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  $(HOST_EXPORTS) \
	  (cd $(HOST_SUBDIR)/[+module+] && \
	    $(MAKE) $(FLAGS_TO_PASS) [+extra_make_flags+] check)
	fi
[+ ELSE check +]
check-[+module+]:
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	(cd $(HOST_SUBDIR)/[+module+] && \
	  $(MAKE) $(FLAGS_TO_PASS) [+extra_make_flags+] check)
[+ ENDIF no_check +]
@endif [+module+]

.PHONY: install-[+module+] maybe-install-[+module+]
maybe-install-[+module+]:
@if [+module+]
maybe-install-[+module+]: install-[+module+]
[+ IF no_install +]
install-[+module+]:
[+ ELSE install +]
install-[+module+]: installdirs
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	(cd $(HOST_SUBDIR)/[+module+] && \
	  $(MAKE) $(FLAGS_TO_PASS) [+extra_make_flags+] install)
[+ ENDIF no_install +]
@endif [+module+]

# Other targets (info, dvi, etc.)
[+ FOR recursive_targets +]
.PHONY: maybe-[+make_target+]-[+module+] [+make_target+]-[+module+]
maybe-[+make_target+]-[+module+]:
@if [+module+]
maybe-[+make_target+]-[+module+]: [+make_target+]-[+module+]
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
	$(HOST_EXPORTS) \
	for flag in $(EXTRA_HOST_FLAGS) [+extra_make_flags+]; do \
	  eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	done; \
	echo "Doing [+make_target+] in [+module+]" ; \
	(cd $(HOST_SUBDIR)/[+module+] && \
	  $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
	          "CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	          "RANLIB=$${RANLIB}" \
	          "DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
	          [+make_target+]) \
	  || exit 1
[+ ENDIF +]
@endif [+module+]
[+ ENDFOR recursive_targets +]
[+ ENDFOR host_modules +]

# ---------------------------------------
# Modules which run on the target machine
# ---------------------------------------
[+ FOR target_modules +]
.PHONY: configure-target-[+module+] maybe-configure-target-[+module+]
maybe-configure-target-[+module+]:
@if target-[+module+]
maybe-configure-target-[+module+]: configure-target-[+module+]

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
	$(SET_LIB_PATH) \[+
IF raw_cxx +]
	$(RAW_CXX_TARGET_EXPORTS) \[+
ELSE normal_cxx +]
	$(NORMAL_TARGET_EXPORTS) \[+
ENDIF raw_cxx +]
	echo Configuring in $(TARGET_SUBDIR)/[+module+]; \
	cd "$(TARGET_SUBDIR)/[+module+]" || exit 1; \
	case $(srcdir) in \
	  /* | [A-Za-z]:[\\/]*) topdir=$(srcdir) ;; \
	  *) topdir=`echo $(TARGET_SUBDIR)/[+module+]/ | \
		sed -e 's,\./,,g' -e 's,[^/]*/,../,g' `$(srcdir) ;; \
	esac; \
	srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	libsrcdir="$$s/[+module+]"; \
	rm -f no-such-file || : ; \
	CONFIG_SITE=no-such-file $(SHELL) $${libsrcdir}/configure \
	  $(TARGET_CONFIGARGS) $${srcdiroption} \
	  --with-target-subdir="$(TARGET_SUBDIR)" [+extra_configure_flags+] \
	  || exit 1
@endif target-[+module+]

.PHONY: all-target-[+module+] maybe-all-target-[+module+]
maybe-all-target-[+module+]:
@if target-[+module+]
TARGET-target-[+module+]=[+ IF target +][+target+][+ ELSE +]all[+ ENDIF target +]
maybe-all-target-[+module+]: all-target-[+module+]
all-target-[+module+]: configure-target-[+module+]
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \[+
IF raw_cxx +]
	$(RAW_CXX_TARGET_EXPORTS) \[+
ELSE normal_cxx +]
	$(NORMAL_TARGET_EXPORTS) \[+
ENDIF raw_cxx +]
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(TARGET_FLAGS_TO_PASS) [+
	    IF raw_cxx 
	  +] 'CXX=$$(RAW_CXX_FOR_TARGET)' 'CXX_FOR_TARGET=$$(RAW_CXX_FOR_TARGET)' [+ 
	    ENDIF raw_cxx 
	  +] [+extra_make_flags+] $(TARGET-target-[+module+]))
@endif target-[+module+]

.PHONY: check-target-[+module+] maybe-check-target-[+module+]
maybe-check-target-[+module+]:
@if target-[+module+]
maybe-check-target-[+module+]: check-target-[+module+]
[+ IF no_check +]
# Dummy target for uncheckable module.
check-target-[+module+]:
[+ ELSE check +]
check-target-[+module+]:
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \[+
IF raw_cxx +]
	$(RAW_CXX_TARGET_EXPORTS) \[+
ELSE normal_cxx +]
	$(NORMAL_TARGET_EXPORTS) \[+
ENDIF raw_cxx +]
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(TARGET_FLAGS_TO_PASS) [+
	    IF raw_cxx 
	      +] 'CXX=$$(RAW_CXX_FOR_TARGET)' 'CXX_FOR_TARGET=$$(RAW_CXX_FOR_TARGET)' [+ 
	    ENDIF raw_cxx 
	  +] [+extra_make_flags+] check)
[+ ENDIF no_check +]
@endif target-[+module+]

.PHONY: install-target-[+module+] maybe-install-target-[+module+]
maybe-install-target-[+module+]:
@if target-[+module+]
maybe-install-target-[+module+]: install-target-[+module+]
[+ IF no_install +]
# Dummy target for uninstallable.
install-target-[+module+]:
[+ ELSE install +]
install-target-[+module+]: installdirs
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \[+
IF raw_cxx +]
	$(RAW_CXX_TARGET_EXPORTS) \[+
ELSE normal_cxx +]
	$(NORMAL_TARGET_EXPORTS) \[+
ENDIF raw_cxx +]
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(TARGET_FLAGS_TO_PASS) [+extra_make_flags+] install)
[+ ENDIF no_install +]
@endif target-[+module+]

# Other targets (info, dvi, etc.)
[+ FOR recursive_targets +]
.PHONY: maybe-[+make_target+]-target-[+module+] [+make_target+]-target-[+module+]
maybe-[+make_target+]-target-[+module+]:
@if target-[+module+]
maybe-[+make_target+]-target-[+module+]: [+make_target+]-target-[+module+]
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
	$(SET_LIB_PATH) \[+
IF raw_cxx +]
	$(RAW_CXX_TARGET_EXPORTS) \[+
ELSE normal_cxx +]
	$(NORMAL_TARGET_EXPORTS) \[+
ENDIF raw_cxx +]
	echo "Doing [+make_target+] in $(TARGET_SUBDIR)/[+module+]" ; \
	for flag in $(EXTRA_TARGET_FLAGS); do \
	  eval `echo "$$flag" | sed -e "s|^\([^=]*\)=\(.*\)|\1='\2'; export \1|"`; \
	done; \
	(cd $(TARGET_SUBDIR)/[+module+] && \
	  $(MAKE) $(BASE_FLAGS_TO_PASS) "AR=$${AR}" "AS=$${AS}" \
	          "CC=$${CC}" "CXX=$${CXX}" "LD=$${LD}" "NM=$${NM}" \
	          "RANLIB=$${RANLIB}" \
	          "DLLTOOL=$${DLLTOOL}" "WINDRES=$${WINDRES}" \
	          [+extra_make_flags+] [+make_target+]) \
	  || exit 1
[+ ENDIF +]
@endif target-[+module+]
[+ ENDFOR recursive_targets +]
[+ ENDFOR target_modules +]

# ----------
# GCC module
# ----------

@if gcc-no-bootstrap
# GCC has some more recursive targets, which trigger the old
# (but still current, until the toplevel bootstrap project
# is finished) compiler bootstrapping rules.

GCC_STRAP_TARGETS = bootstrap bootstrap-lean bootstrap2 bootstrap2-lean bootstrap3 bootstrap3-lean bootstrap4 bootstrap4-lean bubblestrap quickstrap cleanstrap restrap
.PHONY: $(GCC_STRAP_TARGETS)
$(GCC_STRAP_TARGETS): all-prebootstrap configure-gcc
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
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
	$(HOST_EXPORTS) \
	echo "$$msg"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) $$compare
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(RECURSE_FLAGS_TO_PASS) all

profiledbootstrap: all-prebootstrap configure-gcc
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	echo "Bootstrapping training compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) stageprofile_build
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	echo "Building feedback based compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) stagefeedback_build
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(RECURSE_FLAGS_TO_PASS) all

.PHONY: cross
cross: all-build all-gas all-ld
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	echo "Building the C and C++ compiler"; \
	cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) LANGUAGES="c c++"
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries"; \
	$(MAKE) $(RECURSE_FLAGS_TO_PASS) LANGUAGES="c c++" all
@endif gcc-no-bootstrap

@if gcc
.PHONY: check-gcc-c++
check-gcc-c++:
	@if [ -f ./gcc/Makefile ] ; then \
	  r=`${PWD_COMMAND}`; export r; \
	  s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	  $(SET_LIB_PATH) \
	  $(HOST_EXPORTS) \
	  (cd gcc && $(MAKE) $(GCC_FLAGS_TO_PASS) check-c++); \
	else \
	  true; \
	fi

.PHONY: check-c++
check-c++: check-target-libstdc++-v3 check-gcc-c++

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
	  $(HOST_EXPORTS) \
	  (cd ./gcc && \
	   $(MAKE) $(GCC_FLAGS_TO_PASS) install); \
	  rm -rf gcc/include; \
	  mv gcc/tmp-include gcc/include 2>/dev/null; \
	else true; fi
@endif gcc

# ---------------------
# GCC bootstrap support
# ---------------------

# We track the current stage (the one in 'gcc') in the stage_current file.
# stage_last instead tracks the stage that was built last.  These targets
# are dummy when toplevel bootstrap is not active.

.PHONY: unstage
unstage:
@if gcc-bootstrap
	@[ -f stage_current ] || $(MAKE) `cat stage_last`-start
@endif gcc-bootstrap

.PHONY: stage
stage:
@if gcc-bootstrap
	@$(MAKE) `cat stage_current`-end
@endif gcc-bootstrap

# We name the build directories for the various stages "stage1-gcc",
# "stage2-gcc","stage3-gcc", etc.

# Since the 'compare' process will fail (on debugging information) if any
# directory names are different, we need to link the gcc directory for
# the previous stage to a constant name ('gcc-prev'), and to make the name of
# the build directories constant as well. For the latter, we use naked names
# like 'gcc', because the scripts in that directory assume it.  We use
# mv on platforms where symlinks to directories do not work or are not
# reliable.

# At the end of the bootstrap, a symlink to 'stage3-gcc' named 'gcc' must
# be kept, so that libraries can find it.  Ick!

# It would be best to preinstall gcc into a staging area (and in the
# future, gather there all prebootstrap packages).  This would allow
# assemblers and linkers can be bootstrapped as well as the compiler
# (both in a combined tree, or separately).  This however requires some
# change to the gcc driver, again in order to avoid comparison failures.

# Bugs: This is crippled when doing parallel make, the `make all-host'
# and `make all-target' phases can be parallelized.


# 'touch' doesn't work right on some platforms.
STAMP = echo timestamp > 

# Only build the C compiler for stage1, because that is the only one that
# we can guarantee will build with the native compiler, and also it is the
# only thing useful for building stage2. STAGE1_CFLAGS (via CFLAGS),
# MAKEINFO and MAKEINFOFLAGS are explicitly passed here to make them
# overrideable (for a bootstrap build stage1 also builds gcc.info).

STAGE1_CFLAGS=@stage1_cflags@
STAGE1_LANGUAGES=@stage1_languages@

# We only want to compare .o files, so set this!
objext = .o

# Flags to pass to stage2 and later makes.
POSTSTAGE1_FLAGS_TO_PASS = \
	CC="$${CC}" CC_FOR_BUILD="$${CC_FOR_BUILD}" \
	STAGE_PREFIX=$$r/stage[+prev+]-gcc/ \
	CFLAGS="$(BOOT_CFLAGS)" \
	ADAC="\$$(CC)"

# For stage 1:
# * We force-disable intermodule optimizations, even if
#   --enable-intermodule was passed, since the installed compiler probably
#   can't handle them.  Luckily, autoconf always respects
#   the last argument when conflicting --enable arguments are passed.
# * Likewise, we force-disable coverage flags, since the installed compiler
#   probably has never heard of them.
# * We build only C (and possibly Ada).

[+ FOR bootstrap-stage +]
.PHONY: stage[+id+]-start stage[+id+]-end

stage[+id+]-start::
	@[ -f stage_current ] && $(MAKE) `cat stage_current`-end || : ; \
	echo stage[+id+] > stage_current ; \
	echo stage[+id+] > stage_last; \
	$(mkinstalldirs) $(HOST_SUBDIR)[+
   FOR host_modules +][+ IF bootstrap +]
@if [+ module +]
	@cd $(HOST_SUBDIR); [ -d stage[+id+]-[+module+] ] || \
	  mkdir stage[+id+]-[+module+]; \
	set stage[+id+]-[+module+] [+module+] ; \
	@CREATE_LINK_TO_DIR@ [+ IF prev +] ; \
	set stage[+prev+]-[+module+] prev-[+module+] ; \
	@CREATE_LINK_TO_DIR@ [+ ENDIF prev +]
@endif [+ module +][+ ENDIF bootstrap +][+ ENDFOR host_modules +]

stage[+id+]-end::
	@rm -f stage_current[+ FOR host_modules +][+ IF bootstrap +]
@if [+ module +]
	@cd $(HOST_SUBDIR); set [+module+] stage[+id+]-[+module+] ; \
	@UNDO_LINK_TO_DIR@ [+ IF prev +] ; \
	set prev-[+module+] stage[+prev+]-[+module+] ; \
	@UNDO_LINK_TO_DIR@ [+ ENDIF prev +]
@endif [+ module +][+ ENDIF bootstrap +][+ ENDFOR host_modules +]

# Bubble a bugfix through all the stages up to stage [+id+].  They
# are remade, but not reconfigured.  The next stage (if any) will not
# be reconfigured as well.
.PHONY: stage[+id+]-bubble
stage[+id+]-bubble:: [+ IF prev +]stage[+prev+]-bubble[+ ENDIF +][+IF lean +]
	@bootstrap_lean@-rm -rf stage[+lean+]-* ; $(STAMP) stage[+lean+]-lean[+ ENDIF lean +]
	@if test -f stage[+id+]-lean [+
	  IF prev +]|| test -f stage[+prev+]-lean [+ ENDIF prev +] ; then \
	  echo Skipping rebuild of stage[+id+] ; \
	else \
	  $(MAKE) $(RECURSE_FLAGS_TO_PASS) NOTPARALLEL= all-stage[+id+]; \
	fi

.PHONY: all-stage[+id+] clean-stage[+id+]
all-stage[+id+]: [+ FOR host_modules +][+ IF bootstrap +]\
  maybe-all-stage[+id+]-[+module+][+
ENDIF bootstrap+] [+ ENDFOR host_modules +]

do-clean: clean-stage[+id+]
clean-stage[+id+]: [+ FOR host_modules +][+ IF bootstrap +]\
  maybe-clean-stage[+id+]-[+module+][+
ENDIF bootstrap+] [+ ENDFOR host_modules +]

[+ FOR host_modules +][+ IF bootstrap +]
.PHONY: configure-stage[+id+]-[+module+] maybe-configure-stage[+id+]-[+module+]
.PHONY: all-stage[+id+]-[+module+] maybe-all-stage[+id+]-[+module+]
.PHONY: clean-stage[+id+]-[+module+] maybe-clean-stage[+id+]-[+module+]

maybe-configure-stage[+id+]-[+module+]:
maybe-all-stage[+id+]-[+module+]:
maybe-clean-stage[+id+]-[+module+]:

@if [+module+]-bootstrap
maybe-configure-stage[+id+]-[+module+]: configure-stage[+id+]-[+module+]
configure-stage[+id+]-[+module+]:
	@$(MAKE) stage[+id+]-start
	@[ -f [+module+]/Makefile ] && exit 0 || : ; \
	r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; [+ IF prev +] \
	$(STAGE_HOST_EXPORTS) [+ ELSE prev +] \
	$(HOST_EXPORTS) [+ ENDIF prev +] \
	echo Configuring stage [+id+] in [+module+] ; \
	cd $(HOST_SUBDIR)/[+module+] || exit 1; \
	case $(srcdir) in \
	  /* | [A-Za-z]:[\\/]*) topdir=$(srcdir) ;; \
	  *) topdir=`echo $(HOST_SUBDIR)/[+module+]/ | \
		sed -e 's,\./,,g' -e 's,[^/]*/,../,g' `$(srcdir) ;; \
	esac; \
	srcdiroption="--srcdir=$${topdir}/[+module+]"; \
	libsrcdir="$$s/[+module+]"; \
	$(SHELL) $${libsrcdir}/configure \
	  $(HOST_CONFIGARGS) $${srcdiroption} \
	  [+stage_configure_flags+] [+extra_configure_flags+]

maybe-all-stage[+id+]-[+module+]: all-stage[+id+]-[+module+]
all-stage[+id+]-[+module+]: configure-stage[+id+]-[+module+]
	@$(MAKE) stage[+id+]-start
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; [+ IF prev +] \
	$(STAGE_HOST_EXPORTS) [+ ELSE prev +] \
	$(HOST_EXPORTS) [+ ENDIF prev +] \
	cd $(HOST_SUBDIR)/[+module+] && \
	$(MAKE) $(FLAGS_TO_PASS) [+ IF prev +] \
		$(POSTSTAGE1_FLAGS_TO_PASS) [+ ENDIF prev +] \
		[+stage_make_flags+] [+extra_make_flags+]

maybe-clean-stage[+id+]-[+module+]: clean-stage[+id+]-[+module+]
clean-stage[+id+]-[+module+]:
	@[ -f [+module+]/Makefile ] || [ -f stage[+id+]-[+module+]/Makefile ] \
	  || exit 0 ; \
	[ -f [+module+]/Makefile ] || $(MAKE) stage[+id+]-start ; \
	cd $(HOST_SUBDIR)/[+module+] && \
	$(MAKE) $(FLAGS_TO_PASS) [+ IF prev +] \
		$(POSTSTAGE1_FLAGS_TO_PASS) [+ ENDIF prev +] \
		[+stage_make_flags+] [+extra_make_flags+] clean
@endif [+module+]-bootstrap

[+ ENDIF bootstrap +][+ ENDFOR host_modules +]

# FIXME: Will not need to be conditional when toplevel bootstrap is the
# only possibility, but now it conflicts with no-bootstrap rules
@if gcc-bootstrap
[+ IF compare-target +]
[+compare-target+]:
	@if test -f stage[+prev+]-lean; then \
	  echo Cannot compare object files as stage [+prev+] was deleted. ; \
	  exit 0 ; \
	fi; \
	[ -f stage_current ] && $(MAKE) `cat stage_current`-end || : ; \
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	rm -f .bad_compare ; \
	cd stage[+id+]-gcc; \
	files=`find . -name "*$(objext)" -print` ; \
	cd .. ; \
	for file in $${files} ; do \
	  f1=$$r/stage[+prev+]-gcc/$$file; f2=$$r/stage[+id+]-gcc/$$file; \
	  @do_compare@ > /dev/null 2>&1; \
	  test $$? -eq 1 && echo $$file differs >> .bad_compare || true; \
	done ; \
	if [ -f .bad_compare ]; then \
	  echo "Bootstrap comparison failure!"; \
	  cat .bad_compare; \
	  exit 1; \
	else \
	  true; \
	fi ; \
	$(STAMP) [+compare-target+][+ IF prev +]
	@bootstrap_lean@-rm -rf stage[+prev+]-* ; $(STAMP) stage[+prev+]-lean[+ ENDIF prev +]
[+ ENDIF compare-target +]

[+ IF bootstrap-target +]
.PHONY: [+bootstrap-target+]
[+bootstrap-target+]: stage[+id+]-bubble [+compare-target+] all
[+ ENDIF bootstrap-target +]

# Rules to wipe a stage and all the following ones, also used for cleanstrap
[+ IF prev +]distclean-stage[+prev+]:: distclean-stage[+id+] [+ ENDIF prev +]
.PHONY: distclean-stage[+id+]
distclean-stage[+id+]::
	[ -f stage_current ] && $(MAKE) `cat stage_current`-end || :
	rm -rf stage[+id+]-* [+
	  IF compare-target +][+compare-target+] [+ ENDIF compare-target +]

[+ IF cleanstrap-target +]
.PHONY: [+cleanstrap-target+]
[+cleanstrap-target+]: distclean [+bootstrap-target+]
[+ ENDIF cleanstrap-target +]
@endif gcc-bootstrap

[+ ENDFOR bootstrap-stage +]

stagefeedback-start::
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	cd stageprofile-gcc && \
	  { find . -type d | sort | sed 's,.*,$(SHELL) '"$$s"'/mkinstalldirs "../gcc/&",' | $(SHELL); } && \
	  { find . -name '*.*da' | sed 's,.*,$(LN) -f "&" "../gcc/&",' | $(SHELL); }

# FIXME: Will not need to be conditional when toplevel bootstrap is the
# only possibility, but now it conflicts with no-bootstrap rules
@if gcc-bootstrap
profiledbootstrap:
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	echo "Bootstrapping the compiler"; \
	$(MAKE) stageprofile-bubble distclean-stagefeedback
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}` ; export s; \
	$(SET_LIB_PATH) \
	echo "Building runtime libraries and training compiler"; \
	$(MAKE) $(BASE_FLAGS_TO_PASS) $(RECURSE_FLAGS) all
	@r=`${PWD_COMMAND}`; export r; \
	s=`cd $(srcdir); ${PWD_COMMAND}`; export s; \
	$(SET_LIB_PATH) \
	$(HOST_EXPORTS) \
	echo "Building feedback based compiler"; \
	$(MAKE) stagefeedback-bubble stagefeedback-end
@endif gcc-bootstrap

@if gcc-bootstrap
NOTPARALLEL = .NOTPARALLEL
$(NOTPARALLEL):
do-distclean: distclean-stage1
@endif gcc-bootstrap

# --------------------------------------
# Dependencies between different modules
# --------------------------------------

# Generic dependencies for target modules on host stuff, especially gcc
[+ FOR target_modules +]
configure-target-[+module+]: maybe-all-gcc
[+ ENDFOR target_modules +]

[+ FOR lang_env_dependencies +]
configure-target-[+module+]: maybe-all-target-newlib maybe-all-target-libgloss
[+ IF cxx +]configure-target-[+module+]: maybe-all-target-libstdc++-v3
[+ ENDIF cxx +][+ ENDFOR lang_env_dependencies +]

# There are two types of dependencies here: 'hard' dependencies, where one
# module simply won't build without the other; and 'soft' dependencies, where
# if the depended-on module is missing, the depending module will do without
# or find a substitute somewhere (perhaps installed).  Soft dependencies
# are made here to depend on a 'maybe-' target.  If you're not sure,
# it's safer to use a soft dependency.

[+ ;; These Scheme functions build the bulk of the dependencies.
   ;; dep-target builds a string like "maybe-all-MODULE_KIND-gcc",
   ;; where "maybe-" is only included if HARD is true, and all-gcc
   ;; is taken from VAR-NAME.
   (define dep-target (lambda (module-kind var-name hard)
      (string-append
         (if hard "" "maybe-")
         (dep-subtarget var-name)
         module-kind
         (dep-module var-name)
      )))

   ;; make-dep builds a dependency from the MODULE and ON AutoGen vars.
   (define make-dep (lambda (module-kind on-kind)
      (string-append
         (dep-target module-kind "module" #t) ": "
         (dep-target on-kind "on" (exist? "hard")))))

   ;; dep-subtarget extracts everything up to the first dash in the given
   ;; AutoGen variable, for example it extracts "all-" out of "all-gcc".
   (define dep-subtarget (lambda (var-name)
      (substring (get var-name) 0 (+ 1 (string-index (get var-name) #\-)))))

   ;; dep-module extracts everything up to the first dash in the given
   ;; AutoGen variable, for example it extracts "gcc" out of "all-gcc".
   (define dep-module (lambda (var-name)
      (substring (get var-name) (+ 1 (string-index (get var-name) #\-)))))

   ;; dep-stage builds a string for the prefix of a bootstrap stage.
   (define dep-stage (lambda ()
      (string-append
	 "stage"
	 (get "id")
	 "-")))

   ;; dep-maybe is the same as the AutoGen expression "- hard 'maybe-'"
   ;; but is written in Scheme.
   (define dep-maybe (lambda ()
      (if (exist? "hard") "" "maybe-")))

   ;; dep-kind returns "normal" if the dependency is on an "install" target,
   ;; or if either module is not bootstrapped.  It returns "bootstrap" for
   ;; configure or build dependencies between bootstrapped modules; it returns
   ;; "prebootstrap" for configure or build dependencies of bootstrapped
   ;; modules on a build module (e.g. all-gcc on all-build-bison).  All this
   ;; is only necessary for host modules.
   (define dep-kind (lambda ()
      (if (and (hash-ref boot-modules (dep-module "module"))
	       (=* (dep-module "on") "build-"))
	  "prebootstrap"

	  (if (or (= (dep-subtarget "on") "install-")
		  (=* (dep-module "on") "target-")
		  (not (hash-ref boot-modules (dep-module "module")))
		  (not (hash-ref boot-modules (dep-module "on"))))
              "normal"
	      "bootstrap"))))

   ;; We now build the hash table that is used by dep-kind.
   (define boot-modules (make-hash-table 113))
+]

[+ FOR host_modules +][+
   (if (exist? "bootstrap")
       (hash-create-handle! boot-modules (get "module") #t))
   "" +][+ ENDFOR host_modules +]

# With all the machinery above in place, it is pretty easy to generate
# dependencies.  Host dependencies are a bit more complex because we have
# to check for bootstrap/prebootstrap dependencies.  To resolve
# prebootstrap dependencies, prebootstrap modules are gathered in
# a hash table.
[+ FOR dependencies +][+ (make-dep "" "") +]
[+ CASE (dep-kind) +]
[+ == "prebootstrap"
     +][+ FOR bootstrap_stage +]
[+ (make-dep (dep-stage) "") +][+
       ENDFOR bootstrap_stage +]
all-prebootstrap: [+ (dep-target "" "on" (exist? "hard")) +]
[+ == "bootstrap"
     +][+ FOR bootstrap_stage +]
[+ (make-dep (dep-stage) (dep-stage)) +][+
       ENDFOR bootstrap_stage +]
[+ ESAC +][+
ENDFOR dependencies +]

# Non-toplevel bootstrap rules must depend on several packages, to be built
# before gcc.  Another wart that will go away, hopefully soon.
@if gcc-no-bootstrap
[+ FOR host_modules +][+
   IF (and (not (= (get "module") "gcc"))
	   (hash-ref boot-modules (get "module"))) +]
all-prebootstrap: maybe-all-[+module+][+
   ENDIF +][+
ENDFOR host_modules +]
@endif gcc-no-bootstrap

GDB_TK = @GDB_TK@
all-gdb: $(gdbnlmrequirements) $(GDB_TK)

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
