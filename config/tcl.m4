#------------------------------------------------------------------------
# SC_PATH_TCLCONFIG --
#
#	Locate the tclConfig.sh file and perform a sanity check on
#	the Tcl compile flags
#
# Arguments:
#	none
#
# Results:
#
#	Adds the following arguments to configure:
#		--with-tcl=...
#
#	Defines the following vars:
#		TCL_BIN_DIR	Full path to the directory containing
#				the tclConfig.sh file
#------------------------------------------------------------------------

AC_DEFUN([SC_PATH_TCLCONFIG], [
    #
    # Ok, lets find the tcl configuration
    # First, look for one uninstalled.
    # the alternative search directory is invoked by --with-tcl
    #

    if test x"${no_tcl}" = x ; then
	# we reset no_tcl in case something fails here
	no_tcl=true
	AC_ARG_WITH(tcl, [  --with-tcl              directory containing tcl configuration (tclConfig.sh)], with_tclconfig=${withval})
	AC_MSG_CHECKING([for Tcl configuration])
	AC_CACHE_VAL(ac_cv_c_tclconfig,[

	    # First check to see if --with-tcl was specified.
	    case "${host}" in
		*-*-cygwin*) platDir="win" ;;
		*) platDir="unix" ;;
	    esac
	    if test x"${with_tclconfig}" != x ; then
		if test -f "${with_tclconfig}/tclConfig.sh" ; then
		    ac_cv_c_tclconfig=`(cd ${with_tclconfig}; pwd)`
		else
		    AC_MSG_ERROR([${with_tclconfig} directory doesn't contain tclConfig.sh])
		fi
	    fi

	    # then check for a private Tcl installation
	    if test x"${ac_cv_c_tclconfig}" = x ; then
		for i in \
			../tcl \
			`ls -dr ../tcl[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ../tcl[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ../tcl[[8-9]].[[0-9]]* 2>/dev/null` \
			../../tcl \
			`ls -dr ../../tcl[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ../../tcl[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ../../tcl[[8-9]].[[0-9]]* 2>/dev/null` \
			../../../tcl \
			`ls -dr ../../../tcl[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ../../../tcl[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ../../../tcl[[8-9]].[[0-9]]* 2>/dev/null` ; do
		    if test -f "$i/$platDir/tclConfig.sh" ; then
			ac_cv_c_tclconfig=`(cd $i/$platDir; pwd)`
			break
		    fi
		done
	    fi

	    # on Darwin, check in Framework installation locations
	    if test "`uname -s`" = "Darwin" -a x"${ac_cv_c_tclconfig}" = x ; then
		for i in `ls -d ~/Library/Frameworks 2>/dev/null` \
			`ls -d /Library/Frameworks 2>/dev/null` \
			`ls -d /Network/Library/Frameworks 2>/dev/null` \
			`ls -d /System/Library/Frameworks 2>/dev/null` \
			; do
		    if test -f "$i/Tcl.framework/tclConfig.sh" ; then
			ac_cv_c_tclconfig=`(cd $i/Tcl.framework; pwd)`
			break
		    fi
		done
	    fi

	    # check in a few common install locations
	    if test x"${ac_cv_c_tclconfig}" = x ; then
		for i in `ls -d ${libdir} 2>/dev/null` \
			`ls -d ${exec_prefix}/lib 2>/dev/null` \
			`ls -d ${prefix}/lib 2>/dev/null` \
			`ls -d /usr/local/lib 2>/dev/null` \
			`ls -d /usr/contrib/lib 2>/dev/null` \
			`ls -d /usr/lib 2>/dev/null` \
			; do
		    if test -f "$i/tclConfig.sh" ; then
			ac_cv_c_tclconfig=`(cd $i; pwd)`
			break
		    fi
		done
	    fi

	    # check in a few other private locations
	    if test x"${ac_cv_c_tclconfig}" = x ; then
		for i in \
			${srcdir}/../tcl \
			`ls -dr ${srcdir}/../tcl[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ${srcdir}/../tcl[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ${srcdir}/../tcl[[8-9]].[[0-9]]* 2>/dev/null` ; do
		    if test -f "$i/$platDir/tclConfig.sh" ; then
		    ac_cv_c_tclconfig=`(cd $i/$platDir; pwd)`
		    break
		fi
		done
	    fi
	])

	if test x"${ac_cv_c_tclconfig}" = x ; then
	    TCL_BIN_DIR="# no Tcl configs found"
	    AC_MSG_WARN([Can't find Tcl configuration definitions])
	    exit 0
	else
	    no_tcl=
	    TCL_BIN_DIR=${ac_cv_c_tclconfig}
	    AC_MSG_RESULT([found ${TCL_BIN_DIR}/tclConfig.sh])
	fi
    fi
])

#------------------------------------------------------------------------
# SC_PATH_TKCONFIG --
#
#	Locate the tkConfig.sh file
#
# Arguments:
#	none
#
# Results:
#
#	Adds the following arguments to configure:
#		--with-tk=...
#
#	Defines the following vars:
#		TK_BIN_DIR	Full path to the directory containing
#				the tkConfig.sh file
#------------------------------------------------------------------------

AC_DEFUN([SC_PATH_TKCONFIG], [
    #
    # Ok, lets find the tk configuration
    # First, look for one uninstalled.
    # the alternative search directory is invoked by --with-tk
    #

    if test x"${no_tk}" = x ; then
	# we reset no_tk in case something fails here
	no_tk=true
	AC_ARG_WITH(tk, [  --with-tk               directory containing tk configuration (tkConfig.sh)], with_tkconfig=${withval})
	AC_MSG_CHECKING([for Tk configuration])
	AC_CACHE_VAL(ac_cv_c_tkconfig,[

	    # First check to see if --with-tkconfig was specified.
	    if test x"${with_tkconfig}" != x ; then
		if test -f "${with_tkconfig}/tkConfig.sh" ; then
		    ac_cv_c_tkconfig=`(cd ${with_tkconfig}; pwd)`
		else
		    AC_MSG_ERROR([${with_tkconfig} directory doesn't contain tkConfig.sh])
		fi
	    fi

	    # then check for a private Tk library
	    case "${host}" in
		*-*-cygwin*) platDir="win" ;;
		*) platDir="unix" ;;
	    esac
	    if test x"${ac_cv_c_tkconfig}" = x ; then
		for i in \
			../tk \
			`ls -dr ../tk[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ../tk[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ../tk[[8-9]].[[0-9]]* 2>/dev/null` \
			../../tk \
			`ls -dr ../../tk[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ../../tk[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ../../tk[[8-9]].[[0-9]]* 2>/dev/null` \
			../../../tk \
			`ls -dr ../../../tk[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ../../../tk[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ../../../tk[[8-9]].[[0-9]]* 2>/dev/null` ; do
		    if test -f "$i/$platDir/tkConfig.sh" ; then
			ac_cv_c_tkconfig=`(cd $i/$platDir; pwd)`
			break
		    fi
		done
	    fi

	    # on Darwin, check in Framework installation locations
	    if test "`uname -s`" = "Darwin" -a x"${ac_cv_c_tkconfig}" = x ; then
		for i in `ls -d ~/Library/Frameworks 2>/dev/null` \
			`ls -d /Library/Frameworks 2>/dev/null` \
			`ls -d /Network/Library/Frameworks 2>/dev/null` \
			`ls -d /System/Library/Frameworks 2>/dev/null` \
			; do
		    if test -f "$i/Tk.framework/tkConfig.sh" ; then
			ac_cv_c_tkconfig=`(cd $i/Tk.framework; pwd)`
			break
		    fi
		done
	    fi

	    # check in a few common install locations
	    if test x"${ac_cv_c_tkconfig}" = x ; then
		for i in `ls -d ${libdir} 2>/dev/null` \
			`ls -d ${exec_prefix}/lib 2>/dev/null` \
			`ls -d ${prefix}/lib 2>/dev/null` \
			`ls -d /usr/local/lib 2>/dev/null` \
			`ls -d /usr/contrib/lib 2>/dev/null` \
			`ls -d /usr/lib 2>/dev/null` \
			; do
		    if test -f "$i/tkConfig.sh" ; then
			ac_cv_c_tkconfig=`(cd $i; pwd)`
			break
		    fi
		done
	    fi
	    # check in a few other private locations
	    if test x"${ac_cv_c_tkconfig}" = x ; then
		for i in \
			${srcdir}/../tk \
			`ls -dr ${srcdir}/../tk[[8-9]].[[0-9]].[[0-9]]* 2>/dev/null` \
			`ls -dr ${srcdir}/../tk[[8-9]].[[0-9]] 2>/dev/null` \
			`ls -dr ${srcdir}/../tk[[8-9]].[[0-9]]* 2>/dev/null` ; do
		    if test -f "$i/$platDir/tkConfig.sh" ; then
			ac_cv_c_tkconfig=`(cd $i/$platDir; pwd)`
			break
		    fi
		done
	    fi
	])

	if test x"${ac_cv_c_tkconfig}" = x ; then
	    TK_BIN_DIR="# no Tk configs found"
	    AC_MSG_WARN([Can't find Tk configuration definitions])
	    exit 0
	else
	    no_tk=
	    TK_BIN_DIR=${ac_cv_c_tkconfig}
	    AC_MSG_RESULT([found ${TK_BIN_DIR}/tkConfig.sh])
	fi
    fi
])

#------------------------------------------------------------------------
# SC_LOAD_TCLCONFIG --
#
#	Load the tclConfig.sh file
#
# Arguments:
#	
#	Requires the following vars to be set:
#		TCL_BIN_DIR
#
# Results:
#
#	Subst the following vars:
#		TCL_BIN_DIR
#		TCL_SRC_DIR
#		TCL_LIB_FILE
#
#------------------------------------------------------------------------

AC_DEFUN([SC_LOAD_TCLCONFIG], [
    AC_MSG_CHECKING([for existence of ${TCL_BIN_DIR}/tclConfig.sh])

    if test -f "${TCL_BIN_DIR}/tclConfig.sh" ; then
        AC_MSG_RESULT([loading])
	. ${TCL_BIN_DIR}/tclConfig.sh
    else
        AC_MSG_RESULT([could not find ${TCL_BIN_DIR}/tclConfig.sh])
    fi

    # eval is required to do the TCL_DBGX substitution
    eval "TCL_LIB_FILE=\"${TCL_LIB_FILE}\""
    eval "TCL_STUB_LIB_FILE=\"${TCL_STUB_LIB_FILE}\""

    # If the TCL_BIN_DIR is the build directory (not the install directory),
    # then set the common variable name to the value of the build variables.
    # For example, the variable TCL_LIB_SPEC will be set to the value
    # of TCL_BUILD_LIB_SPEC. An extension should make use of TCL_LIB_SPEC
    # instead of TCL_BUILD_LIB_SPEC since it will work with both an
    # installed and uninstalled version of Tcl.
    if test -f ${TCL_BIN_DIR}/Makefile ; then
        TCL_LIB_SPEC=${TCL_BUILD_LIB_SPEC}
        TCL_STUB_LIB_SPEC=${TCL_BUILD_STUB_LIB_SPEC}
        TCL_STUB_LIB_PATH=${TCL_BUILD_STUB_LIB_PATH}
    elif test "`uname -s`" = "Darwin"; then
	# If Tcl was built as a framework, attempt to use the libraries
	# from the framework at the given location so that linking works
	# against Tcl.framework installed in an arbitary location.
	case ${TCL_DEFS} in
	    *TCL_FRAMEWORK*)
		if test -f ${TCL_BIN_DIR}/${TCL_LIB_FILE}; then
		    for i in "`cd ${TCL_BIN_DIR}; pwd`" \
			     "`cd ${TCL_BIN_DIR}/../..; pwd`"; do
			if test "`basename "$i"`" = "${TCL_LIB_FILE}.framework"; then
			    TCL_LIB_SPEC="-F`dirname "$i"` -framework ${TCL_LIB_FILE}"
			    break
			fi
		    done
		fi
		if test -f ${TCL_BIN_DIR}/${TCL_STUB_LIB_FILE}; then
		    TCL_STUB_LIB_SPEC="-L${TCL_BIN_DIR} ${TCL_STUB_LIB_FLAG}"
		    TCL_STUB_LIB_PATH="${TCL_BIN_DIR}/${TCL_STUB_LIB_FILE}"
		fi
		;;
	esac
    fi

    # eval is required to do the TCL_DBGX substitution
    eval "TCL_LIB_FLAG=\"${TCL_LIB_FLAG}\""
    eval "TCL_LIB_SPEC=\"${TCL_LIB_SPEC}\""
    eval "TCL_STUB_LIB_FLAG=\"${TCL_STUB_LIB_FLAG}\""
    eval "TCL_STUB_LIB_SPEC=\"${TCL_STUB_LIB_SPEC}\""

    AC_SUBST(TCL_VERSION)
    AC_SUBST(TCL_PATCH_LEVEL)
    AC_SUBST(TCL_BIN_DIR)
    AC_SUBST(TCL_SRC_DIR)

    AC_SUBST(TCL_LIB_FILE)
    AC_SUBST(TCL_LIB_FLAG)
    AC_SUBST(TCL_LIB_SPEC)

    AC_SUBST(TCL_STUB_LIB_FILE)
    AC_SUBST(TCL_STUB_LIB_FLAG)
    AC_SUBST(TCL_STUB_LIB_SPEC)
])

#------------------------------------------------------------------------
# SC_LOAD_TKCONFIG --
#
#	Load the tkConfig.sh file
#
# Arguments:
#	
#	Requires the following vars to be set:
#		TK_BIN_DIR
#
# Results:
#
#	Sets the following vars that should be in tkConfig.sh:
#		TK_BIN_DIR
#------------------------------------------------------------------------

AC_DEFUN([SC_LOAD_TKCONFIG], [
    AC_MSG_CHECKING([for existence of ${TK_BIN_DIR}/tkConfig.sh])

    if test -f "${TK_BIN_DIR}/tkConfig.sh" ; then
        AC_MSG_RESULT([loading])
	. ${TK_BIN_DIR}/tkConfig.sh
    else
        AC_MSG_RESULT([could not find ${TK_BIN_DIR}/tkConfig.sh])
    fi

    # eval is required to do the TK_DBGX substitution
    eval "TK_LIB_FILE=\"${TK_LIB_FILE}\""
    eval "TK_STUB_LIB_FILE=\"${TK_STUB_LIB_FILE}\""

    # If the TK_BIN_DIR is the build directory (not the install directory),
    # then set the common variable name to the value of the build variables.
    # For example, the variable TK_LIB_SPEC will be set to the value
    # of TK_BUILD_LIB_SPEC. An extension should make use of TK_LIB_SPEC
    # instead of TK_BUILD_LIB_SPEC since it will work with both an
    # installed and uninstalled version of Tcl.
    if test -f ${TK_BIN_DIR}/Makefile ; then
        TK_LIB_SPEC=${TK_BUILD_LIB_SPEC}
        TK_STUB_LIB_SPEC=${TK_BUILD_STUB_LIB_SPEC}
        TK_STUB_LIB_PATH=${TK_BUILD_STUB_LIB_PATH}
    elif test "`uname -s`" = "Darwin"; then
	# If Tk was built as a framework, attempt to use the libraries
	# from the framework at the given location so that linking works
	# against Tk.framework installed in an arbitary location.
	case ${TK_DEFS} in
	    *TK_FRAMEWORK*)
		if test -f ${TK_BIN_DIR}/${TK_LIB_FILE}; then
		    for i in "`cd ${TK_BIN_DIR}; pwd`" \
			     "`cd ${TK_BIN_DIR}/../..; pwd`"; do
			if test "`basename "$i"`" = "${TK_LIB_FILE}.framework"; then
			    TK_LIB_SPEC="-F`dirname "$i"` -framework ${TK_LIB_FILE}"
			    break
			fi
		    done
		fi
		if test -f ${TK_BIN_DIR}/${TK_STUB_LIB_FILE}; then
		    TK_STUB_LIB_SPEC="-L${TK_BIN_DIR} ${TK_STUB_LIB_FLAG}"
		    TK_STUB_LIB_PATH="${TK_BIN_DIR}/${TK_STUB_LIB_FILE}"
		fi
		;;
	esac
    fi

    # eval is required to do the TK_DBGX substitution
    eval "TK_LIB_FLAG=\"${TK_LIB_FLAG}\""
    eval "TK_LIB_SPEC=\"${TK_LIB_SPEC}\""
    eval "TK_STUB_LIB_FLAG=\"${TK_STUB_LIB_FLAG}\""
    eval "TK_STUB_LIB_SPEC=\"${TK_STUB_LIB_SPEC}\""

    AC_SUBST(TK_VERSION)
    AC_SUBST(TK_BIN_DIR)
    AC_SUBST(TK_SRC_DIR)

    AC_SUBST(TK_LIB_FILE)
    AC_SUBST(TK_LIB_FLAG)
    AC_SUBST(TK_LIB_SPEC)

    AC_SUBST(TK_STUB_LIB_FILE)
    AC_SUBST(TK_STUB_LIB_FLAG)
    AC_SUBST(TK_STUB_LIB_SPEC)
])

#------------------------------------------------------------------------
# SC_PROG_TCLSH
#	Locate a tclsh shell installed on the system path. This macro
#	will only find a Tcl shell that already exists on the system.
#	It will not find a Tcl shell in the Tcl build directory or
#	a Tcl shell that has been installed from the Tcl build directory.
#	If a Tcl shell can't be located on the PATH, then TCLSH_PROG will
#	be set to "". Extensions should take care not to create Makefile
#	rules that are run by default and depend on TCLSH_PROG. An
#	extension can't assume that an executable Tcl shell exists at
#	build time.
#
# Arguments
#	none
#
# Results
#	Subst's the following values:
#		TCLSH_PROG
#------------------------------------------------------------------------

AC_DEFUN([SC_PROG_TCLSH], [
    AC_MSG_CHECKING([for tclsh])
    AC_CACHE_VAL(ac_cv_path_tclsh, [
	search_path=`echo ${PATH} | sed -e 's/:/ /g'`
	for dir in $search_path ; do
	    for j in `ls -r $dir/tclsh[[8-9]]* 2> /dev/null` \
		    `ls -r $dir/tclsh* 2> /dev/null` ; do
		if test x"$ac_cv_path_tclsh" = x ; then
		    if test -f "$j" ; then
			ac_cv_path_tclsh=$j
			break
		    fi
		fi
	    done
	done
    ])

    if test -f "$ac_cv_path_tclsh" ; then
	TCLSH_PROG="$ac_cv_path_tclsh"
	AC_MSG_RESULT([$TCLSH_PROG])
    else
	# It is not an error if an installed version of Tcl can't be located.
	TCLSH_PROG=""
	AC_MSG_RESULT([No tclsh found on PATH])
    fi
    AC_SUBST(TCLSH_PROG)
])

#------------------------------------------------------------------------
# SC_BUILD_TCLSH
#	Determine the fully qualified path name of the tclsh executable
#	in the Tcl build directory. This macro will correctly determine
#	the name of the tclsh executable even if tclsh has not yet
#	been built in the build directory. The build tclsh must be used
#	when running tests from an extension build directory. It is not
#	correct to use the TCLSH_PROG in cases like this.
#
# Arguments
#	none
#
# Results
#	Subst's the following values:
#		BUILD_TCLSH
#------------------------------------------------------------------------

AC_DEFUN([SC_BUILD_TCLSH], [
    AC_MSG_CHECKING([for tclsh in Tcl build directory])
    BUILD_TCLSH=${TCL_BIN_DIR}/tclsh
    AC_MSG_RESULT([$BUILD_TCLSH])
    AC_SUBST(BUILD_TCLSH)
])

#------------------------------------------------------------------------
# SC_ENABLE_SHARED --
#
#	Allows the building of shared libraries
#
# Arguments:
#	none
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-shared=yes|no
#
#	Defines the following vars:
#		STATIC_BUILD	Used for building import/export libraries
#				on Windows.
#
#	Sets the following vars:
#		SHARED_BUILD	Value of 1 or 0
#------------------------------------------------------------------------

AC_DEFUN([SC_ENABLE_SHARED], [
    AC_MSG_CHECKING([how to build libraries])
    AC_ARG_ENABLE(shared,
	[  --enable-shared         build and link with shared libraries [--enable-shared]],
	[tcl_ok=$enableval], [tcl_ok=yes])

    if test "${enable_shared+set}" = set; then
	enableval="$enable_shared"
	tcl_ok=$enableval
    else
	tcl_ok=yes
    fi

    if test "$tcl_ok" = "yes" ; then
	AC_MSG_RESULT([shared])
	SHARED_BUILD=1
    else
	AC_MSG_RESULT([static])
	SHARED_BUILD=0
	AC_DEFINE(STATIC_BUILD)
    fi
])

#------------------------------------------------------------------------
# SC_ENABLE_FRAMEWORK --
#
#	Allows the building of shared libraries into frameworks
#
# Arguments:
#	none
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-framework=yes|no
#
#	Sets the following vars:
#		FRAMEWORK_BUILD	Value of 1 or 0
#------------------------------------------------------------------------

AC_DEFUN([SC_ENABLE_FRAMEWORK], [
    if test "`uname -s`" = "Darwin" ; then
	AC_MSG_CHECKING([how to package libraries])
	AC_ARG_ENABLE(framework,
	    [  --enable-framework      package shared libraries in MacOSX frameworks [--disable-framework]],
	    [enable_framework=$enableval], [enable_framework=no])
	if test $enable_framework = yes; then
	    if test $SHARED_BUILD = 0; then
		AC_MSG_WARN([Frameworks can only be built if --enable-shared is yes])
		enable_framework=no
	    fi
	    if test $tcl_corefoundation = no; then
		AC_MSG_WARN([Frameworks can only be used when CoreFoundation is available])
		enable_framework=no
	    fi
	fi
	if test $enable_framework = yes; then
	    AC_MSG_RESULT([framework])
	    FRAMEWORK_BUILD=1
	else
	    if test $SHARED_BUILD = 1; then
		AC_MSG_RESULT([shared library])
	    else
		AC_MSG_RESULT([static library])
	    fi
	    FRAMEWORK_BUILD=0
	fi
    fi
])

#------------------------------------------------------------------------
# SC_ENABLE_THREADS --
#
#	Specify if thread support should be enabled.  TCL_THREADS is
#	checked so that if you are compiling an extension against a
#	threaded core, your extension must be compiled threaded as well.
#
# Arguments:
#	none
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-threads
#
#	Sets the following vars:
#		THREADS_LIBS	Thread library(s)
#
#	Defines the following vars:
#		TCL_THREADS
#		_REENTRANT
#		_THREAD_SAFE
#
#------------------------------------------------------------------------

AC_DEFUN([SC_ENABLE_THREADS], [
    AC_ARG_ENABLE(threads, [  --enable-threads        build with threads],
	[tcl_ok=$enableval], [tcl_ok=no])

    if test "${TCL_THREADS}" = 1; then
	tcl_threaded_core=1;
    fi

    if test "$tcl_ok" = "yes" -o "${TCL_THREADS}" = 1; then
	TCL_THREADS=1
	# USE_THREAD_ALLOC tells us to try the special thread-based
	# allocator that significantly reduces lock contention
	AC_DEFINE(USE_THREAD_ALLOC)
	AC_DEFINE(_REENTRANT)
	if test "`uname -s`" = "SunOS" ; then
	    AC_DEFINE(_POSIX_PTHREAD_SEMANTICS)
	fi
	AC_DEFINE(_THREAD_SAFE)
	AC_CHECK_LIB(pthread,pthread_mutex_init,tcl_ok=yes,tcl_ok=no)
	if test "$tcl_ok" = "no"; then
	    # Check a little harder for __pthread_mutex_init in the same
	    # library, as some systems hide it there until pthread.h is
	    # defined.  We could alternatively do an AC_TRY_COMPILE with
	    # pthread.h, but that will work with libpthread really doesn't
	    # exist, like AIX 4.2.  [Bug: 4359]
	    AC_CHECK_LIB(pthread, __pthread_mutex_init,
		tcl_ok=yes, tcl_ok=no)
	fi

	if test "$tcl_ok" = "yes"; then
	    # The space is needed
	    THREADS_LIBS=" -lpthread"
	else
	    AC_CHECK_LIB(pthreads, pthread_mutex_init,
		tcl_ok=yes, tcl_ok=no)
	    if test "$tcl_ok" = "yes"; then
		# The space is needed
		THREADS_LIBS=" -lpthreads"
	    else
		AC_CHECK_LIB(c, pthread_mutex_init,
		    tcl_ok=yes, tcl_ok=no)
		if test "$tcl_ok" = "no"; then
		    AC_CHECK_LIB(c_r, pthread_mutex_init,
			tcl_ok=yes, tcl_ok=no)
		    if test "$tcl_ok" = "yes"; then
			# The space is needed
			THREADS_LIBS=" -pthread"
		    else
			TCL_THREADS=0
			AC_MSG_WARN([Don't know how to find pthread lib on your system - you must disable thread support or edit the LIBS in the Makefile...])
		    fi
		fi
	    fi
	fi

	# Does the pthread-implementation provide
	# 'pthread_attr_setstacksize' ?

	ac_saved_libs=$LIBS
	LIBS="$LIBS $THREADS_LIBS"
	AC_CHECK_FUNCS(pthread_attr_setstacksize)
	AC_CHECK_FUNCS(pthread_atfork)
	LIBS=$ac_saved_libs
    else
	TCL_THREADS=0
    fi
    # Do checking message here to not mess up interleaved configure output
    AC_MSG_CHECKING([for building with threads])
    if test "${TCL_THREADS}" = 1; then
	AC_DEFINE(TCL_THREADS, 1, [Are we building with threads enabled?])
	if test "${tcl_threaded_core}" = 1; then
	    AC_MSG_RESULT([yes (threaded core)])
	else
	    AC_MSG_RESULT([yes])
	fi
    else
	AC_MSG_RESULT([no (default)])
    fi

    AC_SUBST(TCL_THREADS)
])

#------------------------------------------------------------------------
# SC_ENABLE_SYMBOLS --
#
#	Specify if debugging symbols should be used.
#	Memory (TCL_MEM_DEBUG) and compile (TCL_COMPILE_DEBUG) debugging
#	can also be enabled.
#
# Arguments:
#	none
#	
#	Requires the following vars to be set in the Makefile:
#		CFLAGS_DEBUG
#		CFLAGS_OPTIMIZE
#		LDFLAGS_DEBUG
#		LDFLAGS_OPTIMIZE
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-symbols
#
#	Defines the following vars:
#		CFLAGS_DEFAULT	Sets to $(CFLAGS_DEBUG) if true
#				Sets to $(CFLAGS_OPTIMIZE) if false
#		LDFLAGS_DEFAULT	Sets to $(LDFLAGS_DEBUG) if true
#				Sets to $(LDFLAGS_OPTIMIZE) if false
#		DBGX		Debug library extension
#
#------------------------------------------------------------------------

AC_DEFUN([SC_ENABLE_SYMBOLS], [
    AC_MSG_CHECKING([for build with symbols])
    AC_ARG_ENABLE(symbols, [  --enable-symbols        build with debugging symbols [--disable-symbols]],    [tcl_ok=$enableval], [tcl_ok=no])
# FIXME: Currently, LDFLAGS_DEFAULT is not used, it should work like CFLAGS_DEFAULT.
    if test "$tcl_ok" = "no"; then
	CFLAGS_DEFAULT='$(CFLAGS_OPTIMIZE)'
	LDFLAGS_DEFAULT='$(LDFLAGS_OPTIMIZE)'
	DBGX=""
	AC_MSG_RESULT([no])
    else
	CFLAGS_DEFAULT='$(CFLAGS_DEBUG)'
	LDFLAGS_DEFAULT='$(LDFLAGS_DEBUG)'
	DBGX=g
	if test "$tcl_ok" = "yes"; then
	    AC_MSG_RESULT([yes (standard debugging)])
	fi
    fi
    AC_SUBST(CFLAGS_DEFAULT)
    AC_SUBST(LDFLAGS_DEFAULT)

    if test "$tcl_ok" = "mem" -o "$tcl_ok" = "all"; then
	AC_DEFINE(TCL_MEM_DEBUG)
    fi

    if test "$tcl_ok" = "compile" -o "$tcl_ok" = "all"; then
	AC_DEFINE(TCL_COMPILE_DEBUG)
	AC_DEFINE(TCL_COMPILE_STATS)
    fi

    if test "$tcl_ok" != "yes" -a "$tcl_ok" != "no"; then
	if test "$tcl_ok" = "all"; then
	    AC_MSG_RESULT([enabled symbols mem compile debugging])
	else
	    AC_MSG_RESULT([enabled $tcl_ok debugging])
	fi
    fi
])

#------------------------------------------------------------------------
# SC_ENABLE_LANGINFO --
#
#	Allows use of modern nl_langinfo check for better l10n.
#	This is only relevant for Unix.
#
# Arguments:
#	none
#	
# Results:
#
#	Adds the following arguments to configure:
#		--enable-langinfo=yes|no (default is yes)
#
#	Defines the following vars:
#		HAVE_LANGINFO	Triggers use of nl_langinfo if defined.
#
#------------------------------------------------------------------------

AC_DEFUN([SC_ENABLE_LANGINFO], [
    AC_ARG_ENABLE(langinfo,
	[  --enable-langinfo	  use nl_langinfo if possible to determine
			  encoding at startup, otherwise use old heuristic],
	[langinfo_ok=$enableval], [langinfo_ok=yes])

    HAVE_LANGINFO=0
    if test "$langinfo_ok" = "yes"; then
	AC_CHECK_HEADER(langinfo.h,[langinfo_ok=yes],[langinfo_ok=no])
    fi
    AC_MSG_CHECKING([whether to use nl_langinfo])
    if test "$langinfo_ok" = "yes"; then
	AC_CACHE_VAL(tcl_cv_langinfo_h, [
	    AC_TRY_COMPILE([#include <langinfo.h>], [nl_langinfo(CODESET);],
		    [tcl_cv_langinfo_h=yes],[tcl_cv_langinfo_h=no])])
	AC_MSG_RESULT([$tcl_cv_langinfo_h])
	if test $tcl_cv_langinfo_h = yes; then
	    AC_DEFINE(HAVE_LANGINFO)
	fi
    else 
	AC_MSG_RESULT([$langinfo_ok])
    fi
])

#--------------------------------------------------------------------
# SC_CONFIG_MANPAGES
#	
#	Decide whether to use symlinks for linking the manpages,
#	whether to compress the manpages after installation, and
#	whether to add a package name suffix to the installed
#	manpages to avoidfile name clashes.
#	If compression is enabled also find out what file name suffix
#	the given compression program is using.
#
# Arguments:
#	none
#
# Results:
#
#	Adds the following arguments to configure:
#		--enable-man-symlinks
#		--enable-man-compression=PROG
#		--enable-man-suffix[=STRING]
#
#	Defines the following variable:
#
#	MAN_FLAGS -	The apropriate flags for installManPage
#			according to the user's selection.
#
#--------------------------------------------------------------------

AC_DEFUN([SC_CONFIG_MANPAGES], [
    AC_MSG_CHECKING([whether to use symlinks for manpages])
    AC_ARG_ENABLE(man-symlinks,
	    [  --enable-man-symlinks   use symlinks for the manpages],
	test "$enableval" != "no" && MAN_FLAGS="$MAN_FLAGS --symlinks",
	enableval="no")
    AC_MSG_RESULT([$enableval])

    AC_MSG_CHECKING([whether to compress the manpages])
    AC_ARG_ENABLE(man-compression,
	    [  --enable-man-compression=PROG
		      compress the manpages with PROG],
	[case $enableval in
	    yes) AC_MSG_ERROR([missing argument to --enable-man-compression]);;
	    no)  ;;
	    *)   MAN_FLAGS="$MAN_FLAGS --compress $enableval";;
	esac],
	enableval="no")
    AC_MSG_RESULT([$enableval])
    if test "$enableval" != "no"; then
	AC_MSG_CHECKING([for compressed file suffix])
	touch TeST
	$enableval TeST
	Z=`ls TeST* | sed 's/^....//'`
	rm -f TeST*
	MAN_FLAGS="$MAN_FLAGS --extension $Z"
	AC_MSG_RESULT([$Z])
    fi

    AC_MSG_CHECKING([whether to add a package name suffix for the manpages])
    AC_ARG_ENABLE(man-suffix,
	    [  --enable-man-suffix=STRING
		      use STRING as a suffix to manpage file names
		      (default: $1)],
	[case $enableval in
	    yes) enableval="$1" MAN_FLAGS="$MAN_FLAGS --suffix $enableval";;
	    no)  ;;
	    *)   MAN_FLAGS="$MAN_FLAGS --suffix $enableval";;
	esac],
	enableval="no")
    AC_MSG_RESULT([$enableval])

    AC_SUBST(MAN_FLAGS)
])

#--------------------------------------------------------------------
# SC_CONFIG_SYSTEM
#
#	Determine what the system is (some things cannot be easily checked
#	on a feature-driven basis, alas). This can usually be done via the
#	"uname" command, but there are a few systems, like Next, where
#	this doesn't work.
#
# Arguments:
#	none
#
# Results:
#	Defines the following var:
#
#	system -	System/platform/version identification code.
#
#--------------------------------------------------------------------

AC_DEFUN([SC_CONFIG_SYSTEM], [
    AC_CACHE_CHECK([system version], tcl_cv_sys_version, [
	if test -f /usr/lib/NextStep/software_version; then
	    tcl_cv_sys_version=NEXTSTEP-`awk '/3/,/3/' /usr/lib/NextStep/software_version`
	else
	    tcl_cv_sys_version=`uname -s`-`uname -r`
	    if test "$?" -ne 0 ; then
		AC_MSG_WARN([can't find uname command])
		tcl_cv_sys_version=unknown
	    else
		# Special check for weird MP-RAS system (uname returns weird
		# results, and the version is kept in special file).

		if test -r /etc/.relid -a "X`uname -n`" = "X`uname -s`" ; then
		    tcl_cv_sys_version=MP-RAS-`awk '{print $[3]}' /etc/.relid`
		fi
		if test "`uname -s`" = "AIX" ; then
		    tcl_cv_sys_version=AIX-`uname -v`.`uname -r`
		fi
	    fi
	fi
    ])
    system=$tcl_cv_sys_version
])

#--------------------------------------------------------------------
# SC_CONFIG_CFLAGS
#
#	Try to determine the proper flags to pass to the compiler
#	for building shared libraries and other such nonsense.
#
# Arguments:
#	none
#
# Results:
#
#	Defines and substitutes the following vars:
#
#       DL_OBJS -       Name of the object file that implements dynamic
#                       loading for Tcl on this system.
#       DL_LIBS -       Library file(s) to include in tclsh and other base
#                       applications in order for the "load" command to work.
#       LDFLAGS -      Flags to pass to the compiler when linking object
#                       files into an executable application binary such
#                       as tclsh.
#       LD_SEARCH_FLAGS-Flags to pass to ld, such as "-R /usr/local/tcl/lib",
#                       that tell the run-time dynamic linker where to look
#                       for shared libraries such as libtcl.so.  Depends on
#                       the variable LIB_RUNTIME_DIR in the Makefile. Could
#                       be the same as CC_SEARCH_FLAGS if ${CC} is used to link.
#       CC_SEARCH_FLAGS-Flags to pass to ${CC}, such as "-Wl,-rpath,/usr/local/tcl/lib",
#                       that tell the run-time dynamic linker where to look
#                       for shared libraries such as libtcl.so.  Depends on
#                       the variable LIB_RUNTIME_DIR in the Makefile.
#       MAKE_LIB -      Command to execute to build the a library;
#                       differs when building shared or static.
#       MAKE_STUB_LIB -
#                       Command to execute to build a stub library.
#       INSTALL_LIB -   Command to execute to install a library;
#                       differs when building shared or static.
#       INSTALL_STUB_LIB -
#                       Command to execute to install a stub library.
#       STLIB_LD -      Base command to use for combining object files
#                       into a static library.
#       SHLIB_CFLAGS -  Flags to pass to cc when compiling the components
#                       of a shared library (may request position-independent
#                       code, among other things).
#       SHLIB_LD -      Base command to use for combining object files
#                       into a shared library.
#       SHLIB_LD_LIBS - Dependent libraries for the linker to scan when
#                       creating shared libraries.  This symbol typically
#                       goes at the end of the "ld" commands that build
#                       shared libraries. The value of the symbol is
#                       "${LIBS}" if all of the dependent libraries should
#                       be specified when creating a shared library.  If
#                       dependent libraries should not be specified (as on
#                       SunOS 4.x, where they cause the link to fail, or in
#                       general if Tcl and Tk aren't themselves shared
#                       libraries), then this symbol has an empty string
#                       as its value.
#       SHLIB_SUFFIX -  Suffix to use for the names of dynamically loadable
#                       extensions.  An empty string means we don't know how
#                       to use shared libraries on this platform.
# TCL_SHLIB_LD_EXTRAS - Additional element which are added to SHLIB_LD_LIBS
#  TK_SHLIB_LD_EXTRAS   for the build of Tcl and Tk, but not recorded in the
#                       tclConfig.sh, since they are only used for the build
#                       of Tcl and Tk. 
#                       Examples: MacOS X records the library version and
#                       compatibility version in the shared library.  But
#                       of course the Tcl version of this is only used for Tcl.
#       LIB_SUFFIX -    Specifies everything that comes after the "libfoo"
#                       in a static or shared library name, using the $VERSION variable
#                       to put the version in the right place.  This is used
#                       by platforms that need non-standard library names.
#                       Examples:  ${VERSION}.so.1.1 on NetBSD, since it needs
#                       to have a version after the .so, and ${VERSION}.a
#                       on AIX, since a shared library needs to have
#                       a .a extension whereas shared objects for loadable
#                       extensions have a .so extension.  Defaults to
#                       ${VERSION}${SHLIB_SUFFIX}.
#       TCL_NEEDS_EXP_FILE -
#                       1 means that an export file is needed to link to a
#                       shared library.
#       TCL_EXP_FILE -  The name of the installed export / import file which
#                       should be used to link to the Tcl shared library.
#                       Empty if Tcl is unshared.
#       TCL_BUILD_EXP_FILE -
#                       The name of the built export / import file which
#                       should be used to link to the Tcl shared library.
#                       Empty if Tcl is unshared.
#	CFLAGS_DEBUG -
#			Flags used when running the compiler in debug mode
#	CFLAGS_OPTIMIZE -
#			Flags used when running the compiler in optimize mode
#	CFLAGS -	Additional CFLAGS added as necessary (usually 64-bit)
#
#--------------------------------------------------------------------

AC_DEFUN([SC_CONFIG_CFLAGS], [

    # Step 0.a: Enable 64 bit support?

    AC_MSG_CHECKING([if 64bit support is requested])
    AC_ARG_ENABLE(64bit,[  --enable-64bit          enable 64bit support (where applicable)],
	[do64bit=$enableval], [do64bit=no])
    AC_MSG_RESULT([$do64bit])

    # Step 0.b: Enable Solaris 64 bit VIS support?

    AC_MSG_CHECKING([if 64bit Sparc VIS support is requested])
    AC_ARG_ENABLE(64bit-vis,[  --enable-64bit-vis      enable 64bit Sparc VIS support],
	[do64bitVIS=$enableval], [do64bitVIS=no])
    AC_MSG_RESULT([$do64bitVIS])

    if test "$do64bitVIS" = "yes"; then
	# Force 64bit on with VIS
	do64bit=yes
    fi

    # Step 1: set the variable "system" to hold the name and version number
    # for the system.

    SC_CONFIG_SYSTEM

    # Step 2: check for existence of -ldl library.  This is needed because
    # Linux can use either -ldl or -ldld for dynamic loading.

    AC_CHECK_LIB(dl, dlopen, have_dl=yes, have_dl=no)

    # Require ranlib early so we can override it in special cases below.

    AC_REQUIRE([AC_PROG_RANLIB])

    # Step 3: set configuration options based on system name and version.

    do64bit_ok=no
    LDFLAGS_ORIG="$LDFLAGS"
    TCL_EXPORT_FILE_SUFFIX=""
    UNSHARED_LIB_SUFFIX=""
    TCL_TRIM_DOTS='`echo ${VERSION} | tr -d .`'
    ECHO_VERSION='`echo ${VERSION}`'
    TCL_LIB_VERSIONS_OK=ok
    CFLAGS_DEBUG=-g
    CFLAGS_OPTIMIZE=-O
    if test "$GCC" = "yes" ; then
	CFLAGS_WARNING="-Wall -Wno-implicit-int -fno-strict-aliasing"
    else
	CFLAGS_WARNING=""
    fi
    TCL_NEEDS_EXP_FILE=0
    TCL_BUILD_EXP_FILE=""
    TCL_EXP_FILE=""
dnl FIXME: Replace AC_CHECK_PROG with AC_CHECK_TOOL once cross compiling is fixed.
dnl AC_CHECK_TOOL(AR, ar)
    AC_CHECK_PROG(AR, ar, ar)
    if test "${AR}" = "" ; then
	AC_MSG_ERROR([Required archive tool 'ar' not found on PATH.])
    fi
    STLIB_LD='${AR} cr'
    LD_LIBRARY_PATH_VAR="LD_LIBRARY_PATH"
    PLAT_OBJS=""
    PLAT_SRCS=""
    case $system in
	AIX-*)
	    if test "${TCL_THREADS}" = "1" -a "$GCC" != "yes" ; then
		# AIX requires the _r compiler when gcc isn't being used
		case "${CC}" in
		    *_r)
			# ok ...
			;;
		    *)
			CC=${CC}_r
			;;
		esac
		AC_MSG_RESULT([Using $CC for compiling with threads])
	    fi
	    LIBS="$LIBS -lc"
	    SHLIB_CFLAGS=""
	    # Note: need the LIBS below, otherwise Tk won't find Tcl's
	    # symbols when dynamically loaded into tclsh.
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"

	    DL_OBJS="tclLoadDl.o"
	    LD_LIBRARY_PATH_VAR="LIBPATH"

	    # Check to enable 64-bit flags for compiler/linker on AIX 4+
	    if test "$do64bit" = "yes" -a "`uname -v`" -gt "3" ; then
		if test "$GCC" = "yes" ; then
		    AC_MSG_WARN([64bit mode not supported with GCC on $system])
		else 
		    do64bit_ok=yes
		    CFLAGS="$CFLAGS -q64"
		    LDFLAGS="$LDFLAGS -q64"
		    RANLIB="${RANLIB} -X64"
		    AR="${AR} -X64"
		    SHLIB_LD_FLAGS="-b64"
		fi
	    fi

	    if test "`uname -m`" = "ia64" ; then
		# AIX-5 uses ELF style dynamic libraries on IA-64, but not PPC
		SHLIB_LD="/usr/ccs/bin/ld -G -z text"
		# AIX-5 has dl* in libc.so
		DL_LIBS=""
		if test "$GCC" = "yes" ; then
		    CC_SEARCH_FLAGS='-Wl,-R,${LIB_RUNTIME_DIR}'
		else
		    CC_SEARCH_FLAGS='-R${LIB_RUNTIME_DIR}'
		fi
		LD_SEARCH_FLAGS='-R ${LIB_RUNTIME_DIR}'
	    else
		if test "$GCC" = "yes" ; then
		    SHLIB_LD="gcc -shared"
		else
		    SHLIB_LD="/bin/ld -bhalt:4 -bM:SRE -bE:lib.exp -H512 -T512 -bnoentry"
		fi
		SHLIB_LD="${TCL_SRC_DIR}/unix/ldAix ${SHLIB_LD} ${SHLIB_LD_FLAGS}"
		DL_LIBS="-ldl"
		CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
		TCL_NEEDS_EXP_FILE=1
		TCL_EXPORT_FILE_SUFFIX='${VERSION}\$\{DBGX\}.exp'
	    fi

	    # AIX v<=4.1 has some different flags than 4.2+
	    if test "$system" = "AIX-4.1" -o "`uname -v`" -lt "4" ; then
		LIBOBJS="$LIBOBJS tclLoadAix.o"
		DL_LIBS="-lld"
	    fi

	    # On AIX <=v4 systems, libbsd.a has to be linked in to support
	    # non-blocking file IO.  This library has to be linked in after
	    # the MATH_LIBS or it breaks the pow() function.  The way to
	    # insure proper sequencing, is to add it to the tail of MATH_LIBS.
	    # This library also supplies gettimeofday.
	    #
	    # AIX does not have a timezone field in struct tm. When the AIX
	    # bsd library is used, the timezone global and the gettimeofday
	    # methods are to be avoided for timezone deduction instead, we
	    # deduce the timezone by comparing the localtime result on a
	    # known GMT value.

	    AC_CHECK_LIB(bsd, gettimeofday, libbsd=yes, libbsd=no)
	    if test $libbsd = yes; then
	    	MATH_LIBS="$MATH_LIBS -lbsd"
	    	AC_DEFINE(USE_DELTA_FOR_TZ)
	    fi
	    ;;
	BeOS*)
	    SHLIB_CFLAGS="-fPIC"
	    SHLIB_LD="${CC} -nostart"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"

	    #-----------------------------------------------------------
	    # Check for inet_ntoa in -lbind, for BeOS (which also needs
	    # -lsocket, even if the network functions are in -lnet which
	    # is always linked to, for compatibility.
	    #-----------------------------------------------------------
	    AC_CHECK_LIB(bind, inet_ntoa, [LIBS="$LIBS -lbind -lsocket"])
	    ;;
	BSD/OS-2.1*|BSD/OS-3*)
	    SHLIB_CFLAGS=""
	    SHLIB_LD="shlicc -r"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	BSD/OS-4.*)
	    SHLIB_CFLAGS="-export-dynamic -fPIC"
	    SHLIB_LD="cc -shared"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    LDFLAGS="$LDFLAGS -export-dynamic"
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	dgux*)
	    SHLIB_CFLAGS="-K PIC"
	    SHLIB_LD="cc -G"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	HP-UX-*.11.*)
	    # Use updated header definitions where possible
	    AC_DEFINE(_XOPEN_SOURCE)          # Use the XOPEN network library
	    AC_DEFINE(_XOPEN_SOURCE_EXTENDED) # Use the XOPEN network library
	    LIBS="$LIBS -lxnet"               # Use the XOPEN network library

	    if test "`uname -m`" = "ia64" ; then
		SHLIB_SUFFIX=".so"
	    else
		SHLIB_SUFFIX=".sl"
	    fi
	    AC_CHECK_LIB(dld, shl_load, tcl_ok=yes, tcl_ok=no)
	    if test "$tcl_ok" = yes; then
		SHLIB_CFLAGS="+z"
		SHLIB_LD="ld -b"
		SHLIB_LD_LIBS='${LIBS}'
		DL_OBJS="tclLoadShl.o"
		DL_LIBS="-ldld"
		LDFLAGS="$LDFLAGS -Wl,-E"
		CC_SEARCH_FLAGS='-Wl,+s,+b,${LIB_RUNTIME_DIR}:.'
		LD_SEARCH_FLAGS='+s +b ${LIB_RUNTIME_DIR}:.'
		LD_LIBRARY_PATH_VAR="SHLIB_PATH"
	    fi
	    if test "$GCC" = "yes" ; then
		SHLIB_LD="gcc -shared"
		SHLIB_LD_LIBS='${LIBS}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    fi

	    # Users may want PA-RISC 1.1/2.0 portable code - needs HP cc
	    #CFLAGS="$CFLAGS +DAportable"

	    # Check to enable 64-bit flags for compiler/linker
	    if test "$do64bit" = "yes" ; then
		if test "$GCC" = "yes" ; then
		    hpux_arch=`${CC} -dumpmachine`
		    case $hpux_arch in
			hppa64*)
			    # 64-bit gcc in use.  Fix flags for GNU ld.
			    do64bit_ok=yes
			    SHLIB_LD="${CC} -shared"
			    SHLIB_LD_LIBS='${LIBS}'
			    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
			    LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
			    ;;
			*)
			    AC_MSG_WARN([64bit mode not supported with GCC on $system])
			    ;;
		    esac
		else
		    do64bit_ok=yes
		    CFLAGS="$CFLAGS +DD64"
		    LDFLAGS="$LDFLAGS +DD64"
		fi
	    fi
	    ;;
	HP-UX-*.08.*|HP-UX-*.09.*|HP-UX-*.10.*)
	    SHLIB_SUFFIX=".sl"
	    AC_CHECK_LIB(dld, shl_load, tcl_ok=yes, tcl_ok=no)
	    if test "$tcl_ok" = yes; then
		SHLIB_CFLAGS="+z"
		SHLIB_LD="ld -b"
		SHLIB_LD_LIBS=""
		DL_OBJS="tclLoadShl.o"
		DL_LIBS="-ldld"
		LDFLAGS="$LDFLAGS -Wl,-E"
		CC_SEARCH_FLAGS='-Wl,+s,+b,${LIB_RUNTIME_DIR}:.'
		LD_SEARCH_FLAGS='+s +b ${LIB_RUNTIME_DIR}:.'
		LD_LIBRARY_PATH_VAR="SHLIB_PATH"
	    fi
	    ;;
	IRIX-4.*)
	    SHLIB_CFLAGS="-G 0"
	    SHLIB_SUFFIX=".a"
	    SHLIB_LD="echo tclLdAout $CC \{$SHLIB_CFLAGS\} | `pwd`/tclsh -r -G 0"
	    SHLIB_LD_LIBS='${LIBS}'
	    DL_OBJS="tclLoadAout.o"
	    DL_LIBS=""
	    LDFLAGS="$LDFLAGS -Wl,-D,08000000"
	    CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    SHARED_LIB_SUFFIX='${VERSION}\$\{DBGX\}.a'
	    ;;
	IRIX-5.*)
	    SHLIB_CFLAGS=""
	    SHLIB_LD="ld -shared -rdata_shared"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS='-rpath ${LIB_RUNTIME_DIR}'
	    ;;
	IRIX-6.*)
	    SHLIB_CFLAGS=""
	    SHLIB_LD="ld -n32 -shared -rdata_shared"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS='-rpath ${LIB_RUNTIME_DIR}'
	    if test "$GCC" = "yes" ; then
		CFLAGS="$CFLAGS -mabi=n32"
		LDFLAGS="$LDFLAGS -mabi=n32"
	    else
		case $system in
		    IRIX-6.3)
			# Use to build 6.2 compatible binaries on 6.3.
			CFLAGS="$CFLAGS -n32 -D_OLD_TERMIOS"
			;;
		    *)
			CFLAGS="$CFLAGS -n32"
			;;
		esac
		LDFLAGS="$LDFLAGS -n32"
	    fi
	    ;;
	IRIX64-6.*)
	    SHLIB_CFLAGS=""
	    SHLIB_LD="ld -n32 -shared -rdata_shared"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS='-rpath ${LIB_RUNTIME_DIR}'

	    # Check to enable 64-bit flags for compiler/linker

	    if test "$do64bit" = "yes" ; then
	        if test "$GCC" = "yes" ; then
	            AC_MSG_WARN([64bit mode not supported by gcc])
	        else
	            do64bit_ok=yes
	            SHLIB_LD="ld -64 -shared -rdata_shared"
	            CFLAGS="$CFLAGS -64"
	            LDFLAGS="$LDFLAGS -64"
	        fi
	    fi
	    ;;
	Linux*)
	    SHLIB_CFLAGS="-fPIC"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"

	    CFLAGS_OPTIMIZE=-O2
	    # egcs-2.91.66 on Redhat Linux 6.0 generates lots of warnings 
	    # when you inline the string and math operations.  Turn this off to
	    # get rid of the warnings.
	    #CFLAGS_OPTIMIZE="${CFLAGS_OPTIMIZE} -D__NO_STRING_INLINES -D__NO_MATH_INLINES"

	    if test "$have_dl" = yes; then
		SHLIB_LD='${CC} -shared ${CFLAGS} ${LDFLAGS}'
		DL_OBJS="tclLoadDl.o"
		DL_LIBS="-ldl"
		LDFLAGS="$LDFLAGS -Wl,--export-dynamic"
		CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    else
		AC_CHECK_HEADER(dld.h, [
		    SHLIB_LD="ld -shared"
		    DL_OBJS="tclLoadDld.o"
		    DL_LIBS="-ldld"
		    CC_SEARCH_FLAGS=""
		    LD_SEARCH_FLAGS=""])
	    fi
	    if test "`uname -m`" = "alpha" ; then
		CFLAGS="$CFLAGS -mieee"
	    fi
	    if test $do64bit = yes; then
		AC_CACHE_CHECK([if compiler accepts -m64 flag], tcl_cv_cc_m64, [
		    hold_cflags=$CFLAGS
		    CFLAGS="$CFLAGS -m64"
		    AC_TRY_LINK(,, tcl_cv_cc_m64=yes, tcl_cv_cc_m64=no)
		    CFLAGS=$hold_cflags])
		if test $tcl_cv_cc_m64 = yes; then
		    CFLAGS="$CFLAGS -m64"
		    do64bit_ok=yes
		fi
	    fi

	    # The combo of gcc + glibc has a bug related
	    # to inlining of functions like strtod(). The
	    # -fno-builtin flag should address this problem
	    # but it does not work. The -fno-inline flag
	    # is kind of overkill but it works.
	    # Disable inlining only when one of the
	    # files in compat/*.c is being linked in.
	    if test x"${LIBOBJS}" != x ; then
	        CFLAGS="$CFLAGS -fno-inline"
	    fi

	    # XIM peeking works under XFree86.
	    AC_DEFINE(PEEK_XCLOSEIM)

	    ;;
	GNU*)
	    SHLIB_CFLAGS="-fPIC"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"

	    if test "$have_dl" = yes; then
		SHLIB_LD="${CC} -shared"
		DL_OBJS=""
		DL_LIBS="-ldl"
		LDFLAGS="$LDFLAGS -Wl,--export-dynamic"
		CC_SEARCH_FLAGS=""
		LD_SEARCH_FLAGS=""
	    else
		AC_CHECK_HEADER(dld.h, [
		    SHLIB_LD="ld -shared"
		    DL_OBJS=""
		    DL_LIBS="-ldld"
		    CC_SEARCH_FLAGS=""
		    LD_SEARCH_FLAGS=""])
	    fi
	    if test "`uname -m`" = "alpha" ; then
		CFLAGS="$CFLAGS -mieee"
	    fi
	    ;;
	Lynx*)
	    SHLIB_CFLAGS="-fPIC"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    CFLAGS_OPTIMIZE=-02
	    SHLIB_LD="${CC} -shared "
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-mshared -ldl"
	    LD_FLAGS="-Wl,--export-dynamic"
	    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    ;;
	MP-RAS-02*)
	    SHLIB_CFLAGS="-K PIC"
	    SHLIB_LD="cc -G"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	MP-RAS-*)
	    SHLIB_CFLAGS="-K PIC"
	    SHLIB_LD="cc -G"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    LDFLAGS="$LDFLAGS -Wl,-Bexport"
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	NetBSD-*|FreeBSD-[[1-2]].*)
	    # Not available on all versions:  check for include file.
	    AC_CHECK_HEADER(dlfcn.h, [
		# NetBSD/SPARC needs -fPIC, -fpic will not do.
		SHLIB_CFLAGS="-fPIC"
		SHLIB_LD="ld -Bshareable -x"
		SHLIB_LD_LIBS='${LIBS}'
		SHLIB_SUFFIX=".so"
		DL_OBJS="tclLoadDl.o"
		DL_LIBS=""
		CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS='-rpath ${LIB_RUNTIME_DIR}'
		AC_CACHE_CHECK([for ELF], tcl_cv_ld_elf, [
		    AC_EGREP_CPP(yes, [
#ifdef __ELF__
	yes
#endif
		    ], tcl_cv_ld_elf=yes, tcl_cv_ld_elf=no)])
		if test $tcl_cv_ld_elf = yes; then
		    SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.so'
		else
		    SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.so.1.0'
		fi
	    ], [
		SHLIB_CFLAGS=""
		SHLIB_LD="echo tclLdAout $CC \{$SHLIB_CFLAGS\} | `pwd`/tclsh -r"
		SHLIB_LD_LIBS='${LIBS}'
		SHLIB_SUFFIX=".a"
		DL_OBJS="tclLoadAout.o"
		DL_LIBS=""
		CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
		SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.a'
	    ])

	    # FreeBSD doesn't handle version numbers with dots.

	    UNSHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.a'
	    TCL_LIB_VERSIONS_OK=nodots
	    ;;
	OpenBSD-*)
	    case `arch -s` in
	    m88k|vax)
		SHLIB_CFLAGS=""
		SHLIB_LD="echo tclLdAout $CC \{$SHLIB_CFLAGS\} | `pwd`/tclsh -r"
		SHLIB_LD_LIBS='${LIBS}'
		SHLIB_SUFFIX=".a"
		DL_OBJS="tclLoadAout.o"
		DL_LIBS=""
		LDFLAGS=""
		CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
		SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.a'
		;;
	    *)
		# OpenBSD/SPARC[64] needs -fPIC, -fpic will not do.
		case `machine` in
		sparc|sparc64)
		    SHLIB_CFLAGS="-fPIC";;
	        *)
		    SHLIB_CFLAGS="-fpic";;
	        esac
		SHLIB_LD="${CC} -shared ${SHLIB_CFLAGS}"
		SHLIB_LD_LIBS='${LIBS}'
		SHLIB_SUFFIX=".so"
		DL_OBJS="tclLoadDl.o"
		DL_LIBS=""
		CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
		SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.so.1.0'
		AC_CACHE_CHECK([for ELF], tcl_cv_ld_elf, [
		    AC_EGREP_CPP(yes, [
#ifdef __ELF__
	yes
#endif
		    ], tcl_cv_ld_elf=yes, tcl_cv_ld_elf=no)])
		if test $tcl_cv_ld_elf = yes; then
		    LDFLAGS=-Wl,-export-dynamic
		else
		    LDFLAGS=""
	        fi
		;;
	    esac

	    # OpenBSD doesn't do version numbers with dots.
	    UNSHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.a'
	    TCL_LIB_VERSIONS_OK=nodots
	    ;;
	FreeBSD-*)
	    # FreeBSD 3.* and greater have ELF.
	    SHLIB_CFLAGS="-fPIC"
	    SHLIB_LD="ld -Bshareable -x"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    LDFLAGS="$LDFLAGS -export-dynamic"
	    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS='-rpath ${LIB_RUNTIME_DIR}'
	    if test "${TCL_THREADS}" = "1" ; then
		# The -pthread needs to go in the CFLAGS, not LIBS
		LIBS=`echo $LIBS | sed s/-pthread//`
		CFLAGS="$CFLAGS -pthread"
	    	LDFLAGS="$LDFLAGS -pthread"
	    fi
	    case $system in
	    FreeBSD-3.*)
	    	# FreeBSD-3 doesn't handle version numbers with dots.
	    	UNSHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.a'
	    	SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.so'
	    	TCL_LIB_VERSIONS_OK=nodots
		;;
	    esac
	    ;;
	Darwin-*)
	    CFLAGS_OPTIMIZE="-Os"
	    SHLIB_CFLAGS="-fno-common"
	    # To avoid discrepancies between what headers configure sees during
	    # preprocessing tests and compiling tests, move any -isysroot and
	    # -mmacosx-version-min flags from CFLAGS to CPPFLAGS:
	    CPPFLAGS="${CPPFLAGS} `echo " ${CFLAGS}" | \
		awk 'BEGIN {FS=" +-";ORS=" "}; {for (i=2;i<=NF;i++) \
		if ([$]i~/^(isysroot|mmacosx-version-min)/) print "-"[$]i}'`"
	    CFLAGS="`echo " ${CFLAGS}" | \
		awk 'BEGIN {FS=" +-";ORS=" "}; {for (i=2;i<=NF;i++) \
		if (!([$]i~/^(isysroot|mmacosx-version-min)/)) print "-"[$]i}'`"
	    if test $do64bit = yes; then
		case `arch` in
		    ppc)
			AC_CACHE_CHECK([if compiler accepts -arch ppc64 flag],
				tcl_cv_cc_arch_ppc64, [
			    hold_cflags=$CFLAGS
			    CFLAGS="$CFLAGS -arch ppc64 -mpowerpc64 -mcpu=G5"
			    AC_TRY_LINK(,, tcl_cv_cc_arch_ppc64=yes,
				    tcl_cv_cc_arch_ppc64=no)
			    CFLAGS=$hold_cflags])
			if test $tcl_cv_cc_arch_ppc64 = yes; then
			    CFLAGS="$CFLAGS -arch ppc64 -mpowerpc64 -mcpu=G5"
			    do64bit_ok=yes
			fi;;
		    i386)
			AC_CACHE_CHECK([if compiler accepts -arch x86_64 flag],
				tcl_cv_cc_arch_x86_64, [
			    hold_cflags=$CFLAGS
			    CFLAGS="$CFLAGS -arch x86_64"
			    AC_TRY_LINK(,, tcl_cv_cc_arch_x86_64=yes,
				    tcl_cv_cc_arch_x86_64=no)
			    CFLAGS=$hold_cflags])
			if test $tcl_cv_cc_arch_x86_64 = yes; then
			    CFLAGS="$CFLAGS -arch x86_64"
			    do64bit_ok=yes
			fi;;
		    *)
			AC_MSG_WARN([Don't know how enable 64-bit on architecture `arch`]);;
		esac
	    else
		# Check for combined 32-bit and 64-bit fat build
		echo "$CFLAGS " | grep -E -q -- '-arch (ppc64|x86_64) ' && \
		    echo "$CFLAGS " | grep -E -q -- '-arch (ppc|i386) ' && \
		    fat_32_64=yes
	    fi
	    SHLIB_LD='${CC} -dynamiclib ${CFLAGS} ${LDFLAGS}'
	    AC_CACHE_CHECK([if ld accepts -single_module flag], tcl_cv_ld_single_module, [
		hold_ldflags=$LDFLAGS
		LDFLAGS="$LDFLAGS -dynamiclib -Wl,-single_module"
		AC_TRY_LINK(, [int i;], tcl_cv_ld_single_module=yes, tcl_cv_ld_single_module=no)
		LDFLAGS=$hold_ldflags])
	    if test $tcl_cv_ld_single_module = yes; then
		SHLIB_LD="${SHLIB_LD} -Wl,-single_module"
	    fi
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".dylib"
	    DL_OBJS="tclLoadDyld.o"
	    DL_LIBS=""
	    # Don't use -prebind when building for Mac OS X 10.4 or later only:
	    test "`echo "${MACOSX_DEPLOYMENT_TARGET}" | awk -F '10\\.' '{print int([$]2)}'`" -lt 4 -a \
		"`echo "${CPPFLAGS}" | awk -F '-mmacosx-version-min=10\\.' '{print int([$]2)}'`" -lt 4 && \
		LDFLAGS="$LDFLAGS -prebind"
	    LDFLAGS="$LDFLAGS -headerpad_max_install_names"
	    AC_CACHE_CHECK([if ld accepts -search_paths_first flag], tcl_cv_ld_search_paths_first, [
		hold_ldflags=$LDFLAGS
		LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
		AC_TRY_LINK(, [int i;], tcl_cv_ld_search_paths_first=yes, tcl_cv_ld_search_paths_first=no)
		LDFLAGS=$hold_ldflags])
	    if test $tcl_cv_ld_search_paths_first = yes; then
		LDFLAGS="$LDFLAGS -Wl,-search_paths_first"
	    fi
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    LD_LIBRARY_PATH_VAR="DYLD_LIBRARY_PATH"
	    PLAT_OBJS=\$\(MAC\_OSX_OBJS\)
	    PLAT_SRCS=\$\(MAC\_OSX_SRCS\)
	    AC_MSG_CHECKING([whether to use CoreFoundation])
	    AC_ARG_ENABLE(corefoundation, [  --enable-corefoundation use CoreFoundation API [--enable-corefoundation]],
		[tcl_corefoundation=$enableval], [tcl_corefoundation=yes])
	    AC_MSG_RESULT([$tcl_corefoundation])
	    if test $tcl_corefoundation = yes; then
		AC_CACHE_CHECK([for CoreFoundation.framework], tcl_cv_lib_corefoundation, [
		    hold_libs=$LIBS
		    if test "$fat_32_64" = yes; then for v in CFLAGS CPPFLAGS LDFLAGS; do
			# On Tiger there is no 64-bit CF, so remove 64-bit archs
			# from CFLAGS et al. while testing for presence of CF.
			# 64-bit CF is disabled in tclUnixPort.h if necessary.
			eval 'hold_'$v'="$'$v'";'$v'="`echo "$'$v' "|sed -e "s/-arch ppc64 / /g" -e "s/-arch x86_64 / /g"`"'
		    done; fi
		    LIBS="$LIBS -framework CoreFoundation"
		    AC_TRY_LINK([#include <CoreFoundation/CoreFoundation.h>], 
			[CFBundleRef b = CFBundleGetMainBundle();], 
			tcl_cv_lib_corefoundation=yes, tcl_cv_lib_corefoundation=no)
		    if test "$fat_32_64" = yes; then for v in CFLAGS CPPFLAGS LDFLAGS; do
			eval $v'="$hold_'$v'"'
		    done; fi; LIBS=$hold_libs])
		if test $tcl_cv_lib_corefoundation = yes; then
		    LIBS="$LIBS -framework CoreFoundation"
		    AC_DEFINE(HAVE_COREFOUNDATION)
		else
		    tcl_corefoundation=no
		fi
		if test "$fat_32_64" = yes -a $tcl_corefoundation = yes; then
		    AC_CACHE_CHECK([for 64-bit CoreFoundation], tcl_cv_lib_corefoundation_64, [
			for v in CFLAGS CPPFLAGS LDFLAGS; do
			    eval 'hold_'$v'="$'$v'";'$v'="`echo "$'$v' "|sed -e "s/-arch ppc / /g" -e "s/-arch i386 / /g"`"'
			done
			AC_TRY_LINK([#include <CoreFoundation/CoreFoundation.h>], 
			    [CFBundleRef b = CFBundleGetMainBundle();], 
			    tcl_cv_lib_corefoundation_64=yes, tcl_cv_lib_corefoundation_64=no)
			for v in CFLAGS CPPFLAGS LDFLAGS; do
			    eval $v'="$hold_'$v'"'
			done])
		    if test $tcl_cv_lib_corefoundation_64 = no; then
			AC_DEFINE(NO_COREFOUNDATION_64)
		    fi
		fi
	    fi
	    AC_DEFINE(MAC_OSX_TCL)
	    ;;
	NEXTSTEP-*)
	    SHLIB_CFLAGS=""
	    SHLIB_LD="cc -nostdlib -r"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadNext.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	OS/390-*)
	    CFLAGS_OPTIMIZE=""		# Optimizer is buggy
	    AC_DEFINE(_OE_SOCKETS)	# needed in sys/socket.h
	    ;;      
	OSF1-1.0|OSF1-1.1|OSF1-1.2)
	    # OSF/1 1.[012] from OSF, and derivatives, including Paragon OSF/1
	    SHLIB_CFLAGS=""
	    # Hack: make package name same as library name
	    SHLIB_LD='ld -R -export $@:'
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadOSF.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	OSF1-1.*)
	    # OSF/1 1.3 from OSF using ELF, and derivatives, including AD2
	    SHLIB_CFLAGS="-fPIC"
	    if test "$SHARED_BUILD" = "1" ; then
	        SHLIB_LD="ld -shared"
	    else
	        SHLIB_LD="ld -non_shared"
	    fi
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	OSF1-V*)
	    # Digital OSF/1
	    SHLIB_CFLAGS=""
	    if test "$SHARED_BUILD" = "1" ; then
	        SHLIB_LD='ld -shared -expect_unresolved "*"'
	    else
	        SHLIB_LD='ld -non_shared -expect_unresolved "*"'
	    fi
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS='-Wl,-rpath,${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS='-rpath ${LIB_RUNTIME_DIR}'
	    if test "$GCC" = "yes" ; then
		CFLAGS="$CFLAGS -mieee"
            else
		CFLAGS="$CFLAGS -DHAVE_TZSET -std1 -ieee"
	    fi
	    # see pthread_intro(3) for pthread support on osf1, k.furukawa
	    if test "${TCL_THREADS}" = "1" ; then
		CFLAGS="$CFLAGS -DHAVE_PTHREAD_ATTR_SETSTACKSIZE"
		CFLAGS="$CFLAGS -DTCL_THREAD_STACK_MIN=PTHREAD_STACK_MIN*64"
		LIBS=`echo $LIBS | sed s/-lpthreads//`
		if test "$GCC" = "yes" ; then
		    LIBS="$LIBS -lpthread -lmach -lexc"
		else
		    CFLAGS="$CFLAGS -pthread"
		    LDFLAGS="$LDFLAGS -pthread"
		fi
	    fi

	    ;;
	QNX-6*)
	    # QNX RTP
	    # This may work for all QNX, but it was only reported for v6.
	    SHLIB_CFLAGS="-fPIC"
	    SHLIB_LD="ld -Bshareable -x"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    # dlopen is in -lc on QNX
	    DL_LIBS=""
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	RISCos-*)
	    SHLIB_CFLAGS="-G 0"
	    SHLIB_LD="echo tclLdAout $CC \{$SHLIB_CFLAGS\} | `pwd`/tclsh -r -G 0"
	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".a"
	    DL_OBJS="tclLoadAout.o"
	    DL_LIBS=""
	    LDFLAGS="$LDFLAGS -Wl,-D,08000000"
	    CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    ;;
	SCO_SV-3.2*)
	    # Note, dlopen is available only on SCO 3.2.5 and greater. However,
	    # this test works, since "uname -s" was non-standard in 3.2.4 and
	    # below.
	    if test "$GCC" = "yes" ; then
	    	SHLIB_CFLAGS="-fPIC -melf"
	    	LDFLAGS="$LDFLAGS -melf -Wl,-Bexport"
	    else
	    	SHLIB_CFLAGS="-Kpic -belf"
	    	LDFLAGS="$LDFLAGS -belf -Wl,-Bexport"
	    fi
	    SHLIB_LD="ld -G"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS=""
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	SINIX*5.4*)
	    SHLIB_CFLAGS="-K PIC"
	    SHLIB_LD="cc -G"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
	SunOS-4*)
	    SHLIB_CFLAGS="-PIC"
	    SHLIB_LD="ld"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}

	    # SunOS can't handle version numbers with dots in them in library
	    # specs, like -ltcl7.5, so use -ltcl75 instead.  Also, it
	    # requires an extra version number at the end of .so file names.
	    # So, the library has to have a name like libtcl75.so.1.0

	    SHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.so.1.0'
	    UNSHARED_LIB_SUFFIX='${TCL_TRIM_DOTS}\$\{DBGX\}.a'
	    TCL_LIB_VERSIONS_OK=nodots
	    ;;
	SunOS-5.[[0-6]])
	    # Careful to not let 5.10+ fall into this case

	    # Note: If _REENTRANT isn't defined, then Solaris
	    # won't define thread-safe library routines.

	    AC_DEFINE(_REENTRANT)
	    AC_DEFINE(_POSIX_PTHREAD_SEMANTICS)

	    SHLIB_CFLAGS="-KPIC"

	    # Note: need the LIBS below, otherwise Tk won't find Tcl's
	    # symbols when dynamically loaded into tclsh.

	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    if test "$GCC" = "yes" ; then
		SHLIB_LD="$CC -shared"
		CC_SEARCH_FLAGS='-Wl,-R,${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    else
		SHLIB_LD="/usr/ccs/bin/ld -G -z text"
		CC_SEARCH_FLAGS='-R ${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    fi
	    ;;
	SunOS-5*)
	    # Note: If _REENTRANT isn't defined, then Solaris
	    # won't define thread-safe library routines.

	    AC_DEFINE(_REENTRANT)
	    AC_DEFINE(_POSIX_PTHREAD_SEMANTICS)

	    SHLIB_CFLAGS="-KPIC"

	    # Check to enable 64-bit flags for compiler/linker
	    if test "$do64bit" = "yes" ; then
		arch=`isainfo`
		if test "$arch" = "sparcv9 sparc" ; then
			if test "$GCC" = "yes" ; then
			    if test "`gcc -dumpversion | awk -F. '{print [$]1}'`" -lt "3" ; then
				AC_MSG_WARN([64bit mode not supported with GCC < 3.2 on $system])
			    else
				do64bit_ok=yes
				CFLAGS="$CFLAGS -m64 -mcpu=v9"
				LDFLAGS="$LDFLAGS -m64 -mcpu=v9"
				SHLIB_CFLAGS="-fPIC"
			    fi
			else
			    do64bit_ok=yes
			    if test "$do64bitVIS" = "yes" ; then
				CFLAGS="$CFLAGS -xarch=v9a"
			    	LDFLAGS="$LDFLAGS -xarch=v9a"
			    else
				CFLAGS="$CFLAGS -xarch=v9"
			    	LDFLAGS="$LDFLAGS -xarch=v9"
			    fi
			    # Solaris 64 uses this as well
			    #LD_LIBRARY_PATH_VAR="LD_LIBRARY_PATH_64"
			fi
		elif test "$arch" = "amd64 i386" ; then
		    if test "$GCC" = "yes" ; then
			AC_MSG_WARN([64bit mode not supported with GCC on $system])
		    else
			do64bit_ok=yes
			CFLAGS="$CFLAGS -xarch=amd64"
			LDFLAGS="$LDFLAGS -xarch=amd64"
		    fi
		else
		    AC_MSG_WARN([64bit mode not supported for $arch])
		fi
	    fi
	    
	    # Note: need the LIBS below, otherwise Tk won't find Tcl's
	    # symbols when dynamically loaded into tclsh.

	    SHLIB_LD_LIBS='${LIBS}'
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    if test "$GCC" = "yes" ; then
		SHLIB_LD="$CC -shared"
		CC_SEARCH_FLAGS='-Wl,-R,${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
		if test "$do64bit_ok" = "yes" ; then
		    # We need to specify -static-libgcc or we need to
		    # add the path to the sparv9 libgcc.
		    SHLIB_LD="$SHLIB_LD -m64 -mcpu=v9 -static-libgcc"
		    # for finding sparcv9 libgcc, get the regular libgcc
		    # path, remove so name and append 'sparcv9'
		    #v9gcclibdir="`gcc -print-file-name=libgcc_s.so` | ..."
		    #CC_SEARCH_FLAGS="${CC_SEARCH_FLAGS},-R,$v9gcclibdir"
		fi
	    else
		case $system in
		    SunOS-5.[[1-9]][[0-9]]*)
			SHLIB_LD='${CC} -G -z text';;
		    *)
			SHLIB_LD="/usr/ccs/bin/ld -G -z text";;
		esac
		CC_SEARCH_FLAGS='-Wl,-R,${LIB_RUNTIME_DIR}'
		LD_SEARCH_FLAGS='-R ${LIB_RUNTIME_DIR}'
	    fi
	    ;;
	ULTRIX-4.*)
	    SHLIB_CFLAGS="-G 0"
	    SHLIB_SUFFIX=".a"
	    SHLIB_LD="echo tclLdAout $CC \{$SHLIB_CFLAGS\} | `pwd`/tclsh -r -G 0"
	    SHLIB_LD_LIBS='${LIBS}'
	    DL_OBJS="tclLoadAout.o"
	    DL_LIBS=""
	    LDFLAGS="$LDFLAGS -Wl,-D,08000000"
	    CC_SEARCH_FLAGS='-L${LIB_RUNTIME_DIR}'
	    LD_SEARCH_FLAGS=${CC_SEARCH_FLAGS}
	    if test "$GCC" != "yes" ; then
		CFLAGS="$CFLAGS -DHAVE_TZSET -std1"
	    fi
	    ;;
	UNIX_SV* | UnixWare-5*)
	    SHLIB_CFLAGS="-KPIC"
	    SHLIB_LD="cc -G"
	    SHLIB_LD_LIBS=""
	    SHLIB_SUFFIX=".so"
	    DL_OBJS="tclLoadDl.o"
	    DL_LIBS="-ldl"
	    # Some UNIX_SV* systems (unixware 1.1.2 for example) have linkers
	    # that don't grok the -Bexport option.  Test that it does.
	    AC_CACHE_CHECK([for ld accepts -Bexport flag], tcl_cv_ld_Bexport, [
		hold_ldflags=$LDFLAGS
		LDFLAGS="$LDFLAGS -Wl,-Bexport"
		AC_TRY_LINK(, [int i;], tcl_cv_ld_Bexport=yes, tcl_cv_ld_Bexport=no)
	        LDFLAGS=$hold_ldflags])
	    if test $tcl_cv_ld_Bexport = yes; then
		LDFLAGS="$LDFLAGS -Wl,-Bexport"
	    fi
	    CC_SEARCH_FLAGS=""
	    LD_SEARCH_FLAGS=""
	    ;;
    esac

    if test "$do64bit" = "yes" -a "$do64bit_ok" = "no" ; then
	AC_MSG_WARN([64bit support being disabled -- don't know magic for this platform])
    fi

dnl # Add any CPPFLAGS set in the environment to our CFLAGS, but delay doing so
dnl # until the end of configure, as configure's compile and link tests use
dnl # both CPPFLAGS and CFLAGS (unlike our compile and link) but configure's
dnl # preprocessing tests use only CPPFLAGS.
    SC_CONFIG_COMMANDS_PRE([CFLAGS="${CFLAGS} ${CPPFLAGS}"; CPPFLAGS=""])

    # Step 4: If pseudo-static linking is in use (see K. B. Kenny, "Dynamic
    # Loading for Tcl -- What Became of It?".  Proc. 2nd Tcl/Tk Workshop,
    # New Orleans, LA, Computerized Processes Unlimited, 1994), then we need
    # to determine which of several header files defines the a.out file
    # format (a.out.h, sys/exec.h, or sys/exec_aout.h).  At present, we
    # support only a file format that is more or less version-7-compatible. 
    # In particular,
    #	- a.out files must begin with `struct exec'.
    #	- the N_TXTOFF on the `struct exec' must compute the seek address
    #	  of the text segment
    #	- The `struct exec' must contain a_magic, a_text, a_data, a_bss
    #	  and a_entry fields.
    # The following compilation should succeed if and only if either sys/exec.h
    # or a.out.h is usable for the purpose.
    #
    # Note that the modified COFF format used on MIPS Ultrix 4.x is usable; the
    # `struct exec' includes a second header that contains information that
    # duplicates the v7 fields that are needed.

    if test "x$DL_OBJS" = "xtclLoadAout.o" ; then
	AC_CACHE_CHECK([sys/exec.h], tcl_cv_sysexec_h, [
	AC_TRY_COMPILE([#include <sys/exec.h>],[
	    struct exec foo;
	    unsigned long seek;
	    int flag;
#if defined(__mips) || defined(mips)
	    seek = N_TXTOFF (foo.ex_f, foo.ex_o);
#else
	    seek = N_TXTOFF (foo);
#endif
	    flag = (foo.a_magic == OMAGIC);
	    return foo.a_text + foo.a_data + foo.a_bss + foo.a_entry;
    ], tcl_cv_sysexec_h=usable, tcl_cv_sysexec_h=unusable)])
	if test $tcl_cv_sysexec_h = usable; then
	    AC_DEFINE(USE_SYS_EXEC_H)
	else
	    AC_CACHE_CHECK([a.out.h], tcl_cv_aout_h, [
	    AC_TRY_COMPILE([#include <a.out.h>],[
		struct exec foo;
		unsigned long seek;
		int flag;
#if defined(__mips) || defined(mips)
		seek = N_TXTOFF (foo.ex_f, foo.ex_o);
#else
		seek = N_TXTOFF (foo);
#endif
		flag = (foo.a_magic == OMAGIC);
		return foo.a_text + foo.a_data + foo.a_bss + foo.a_entry;
	    ], tcl_cv_aout_h=usable, tcl_cv_aout_h=unusable)])
	    if test $tcl_cv_aout_h = usable; then
		AC_DEFINE(USE_A_OUT_H)
	    else
		AC_CACHE_CHECK([sys/exec_aout.h], tcl_cv_sysexecaout_h, [
		AC_TRY_COMPILE([#include <sys/exec_aout.h>],[
		    struct exec foo;
		    unsigned long seek;
		    int flag;
#if defined(__mips) || defined(mips)
		    seek = N_TXTOFF (foo.ex_f, foo.ex_o);
#else
		    seek = N_TXTOFF (foo);
#endif
		    flag = (foo.a_midmag == OMAGIC);
		    return foo.a_text + foo.a_data + foo.a_bss + foo.a_entry;
		], tcl_cv_sysexecaout_h=usable, tcl_cv_sysexecaout_h=unusable)])
		if test $tcl_cv_sysexecaout_h = usable; then
		    AC_DEFINE(USE_SYS_EXEC_AOUT_H)
		else
		    DL_OBJS=""
		fi
	    fi
	fi
    fi

    # Step 5: disable dynamic loading if requested via a command-line switch.

    AC_ARG_ENABLE(load, [  --disable-load          disallow dynamic loading and "load" command],
	[tcl_ok=$enableval], [tcl_ok=yes])
    if test "$tcl_ok" = "no"; then
	DL_OBJS=""
    fi

    if test "x$DL_OBJS" != "x" ; then
	BUILD_DLTEST="\$(DLTEST_TARGETS)"
    else
	echo "Can't figure out how to do dynamic loading or shared libraries"
	echo "on this system."
	SHLIB_CFLAGS=""
	SHLIB_LD=""
	SHLIB_SUFFIX=""
	DL_OBJS="tclLoadNone.o"
	DL_LIBS=""
	LDFLAGS="$LDFLAGS_ORIG"
	CC_SEARCH_FLAGS=""
	LD_SEARCH_FLAGS=""
	BUILD_DLTEST=""
    fi

    # If we're running gcc, then change the C flags for compiling shared
    # libraries to the right flags for gcc, instead of those for the
    # standard manufacturer compiler.

    if test "$DL_OBJS" != "tclLoadNone.o" ; then
	if test "$GCC" = "yes" ; then
	    case $system in
		AIX-*)
		    ;;
		BSD/OS*)
		    ;;
		IRIX*)
		    ;;
		NetBSD-*|FreeBSD-*|OpenBSD-*)
		    ;;
		Darwin-*)
		    ;;
		RISCos-*)
		    ;;
		SCO_SV-3.2*)
		    ;;
		ULTRIX-4.*)
		    ;;
		*)
		    SHLIB_CFLAGS="-fPIC"
		    ;;
	    esac
	fi
    fi

    if test "$SHARED_LIB_SUFFIX" = "" ; then
	SHARED_LIB_SUFFIX='${VERSION}\$\{DBGX\}${SHLIB_SUFFIX}'
    fi
    if test "$UNSHARED_LIB_SUFFIX" = "" ; then
	UNSHARED_LIB_SUFFIX='${VERSION}\$\{DBGX\}.a'
    fi

    if test "${SHARED_BUILD}" = "1" && test "${SHLIB_SUFFIX}" != "" ; then
        LIB_SUFFIX=${SHARED_LIB_SUFFIX}
        MAKE_LIB='${SHLIB_LD} -o [$]@ ${OBJS} ${SHLIB_LD_LIBS} ${TCL_SHLIB_LD_EXTRAS} ${TK_SHLIB_LD_EXTRAS} ${LD_SEARCH_FLAGS}'
        INSTALL_LIB='$(INSTALL_LIBRARY) $(LIB_FILE) $(LIB_INSTALL_DIR)/$(LIB_FILE)'
    else
        LIB_SUFFIX=${UNSHARED_LIB_SUFFIX}

        if test "$RANLIB" = "" ; then
            MAKE_LIB='$(STLIB_LD) [$]@ ${OBJS}'
            INSTALL_LIB='$(INSTALL_LIBRARY) $(LIB_FILE) $(LIB_INSTALL_DIR)/$(LIB_FILE)'
        else
            MAKE_LIB='${STLIB_LD} [$]@ ${OBJS} ; ${RANLIB} [$]@'
            INSTALL_LIB='$(INSTALL_LIBRARY) $(LIB_FILE) $(LIB_INSTALL_DIR)/$(LIB_FILE) ; (cd $(LIB_INSTALL_DIR) ; $(RANLIB) $(LIB_FILE))'
        fi

dnl        Not at all clear what this was doing in Tcl's configure.in
dnl        or why it was needed was needed. In any event, this sort of
dnl        things needs to be done in the big loop above.
dnl        REMOVE THIS BLOCK LATER! (mdejong)
dnl        case $system in
dnl            BSD/OS*)
dnl                ;;
dnl            AIX-[[1-4]].*)
dnl                ;;
dnl            *)
dnl                SHLIB_LD_LIBS=""
dnl                ;;
dnl        esac
    fi


    # Stub lib does not depend on shared/static configuration
    if test "$RANLIB" = "" ; then
        MAKE_STUB_LIB='${STLIB_LD} [$]@ ${STUB_LIB_OBJS}'
        INSTALL_STUB_LIB='$(INSTALL_LIBRARY) $(STUB_LIB_FILE) $(LIB_INSTALL_DIR)/$(STUB_LIB_FILE)'
    else
        MAKE_STUB_LIB='${STLIB_LD} [$]@ ${STUB_LIB_OBJS} ; ${RANLIB} [$]@'
        INSTALL_STUB_LIB='$(INSTALL_LIBRARY) $(STUB_LIB_FILE) $(LIB_INSTALL_DIR)/$(STUB_LIB_FILE) ; (cd $(LIB_INSTALL_DIR) ; $(RANLIB) $(STUB_LIB_FILE))'
    fi


    AC_SUBST(DL_LIBS)

    AC_SUBST(DL_OBJS)
    AC_SUBST(PLAT_OBJS)
    AC_SUBST(PLAT_SRCS)
    AC_SUBST(CFLAGS)
    AC_SUBST(CFLAGS_DEBUG)
    AC_SUBST(CFLAGS_OPTIMIZE)
    AC_SUBST(CFLAGS_WARNING)

    AC_SUBST(LDFLAGS)
    AC_SUBST(LDFLAGS_DEBUG)
    AC_SUBST(LDFLAGS_OPTIMIZE)
    AC_SUBST(CC_SEARCH_FLAGS)
    AC_SUBST(LD_SEARCH_FLAGS)

    AC_SUBST(STLIB_LD)
    AC_SUBST(SHLIB_LD)
    AC_SUBST(TCL_SHLIB_LD_EXTRAS)
    AC_SUBST(TK_SHLIB_LD_EXTRAS)
    AC_SUBST(SHLIB_LD_LIBS)
    AC_SUBST(SHLIB_CFLAGS)
    AC_SUBST(SHLIB_SUFFIX)

    AC_SUBST(MAKE_LIB)
    AC_SUBST(MAKE_STUB_LIB)
    AC_SUBST(INSTALL_LIB)
    AC_SUBST(INSTALL_STUB_LIB)
    AC_SUBST(RANLIB)
])

#--------------------------------------------------------------------
# SC_SERIAL_PORT
#
#	Determine which interface to use to talk to the serial port.
#	Note that #include lines must begin in leftmost column for
#	some compilers to recognize them as preprocessor directives,
#	and some build environments have stdin not pointing at a
#	pseudo-terminal (usually /dev/null instead.)
#
# Arguments:
#	none
#	
# Results:
#
#	Defines only one of the following vars:
#		HAVE_SYS_MODEM_H
#		USE_TERMIOS
#		USE_TERMIO
#		USE_SGTTY
#
#--------------------------------------------------------------------

AC_DEFUN([SC_SERIAL_PORT], [
    AC_CHECK_HEADERS(sys/modem.h)
    AC_CACHE_CHECK([termios vs. termio vs. sgtty], tcl_cv_api_serial, [
    AC_TRY_RUN([
#include <termios.h>

int main() {
    struct termios t;
    if (tcgetattr(0, &t) == 0) {
	cfsetospeed(&t, 0);
	t.c_cflag |= PARENB | PARODD | CSIZE | CSTOPB;
	return 0;
    }
    return 1;
}], tcl_cv_api_serial=termios, tcl_cv_api_serial=no, tcl_cv_api_serial=no)
    if test $tcl_cv_api_serial = no ; then
	AC_TRY_RUN([
#include <termio.h>

int main() {
    struct termio t;
    if (ioctl(0, TCGETA, &t) == 0) {
	t.c_cflag |= CBAUD | PARENB | PARODD | CSIZE | CSTOPB;
	return 0;
    }
    return 1;
}], tcl_cv_api_serial=termio, tcl_cv_api_serial=no, tcl_cv_api_serial=no)
    fi
    if test $tcl_cv_api_serial = no ; then
	AC_TRY_RUN([
#include <sgtty.h>

int main() {
    struct sgttyb t;
    if (ioctl(0, TIOCGETP, &t) == 0) {
	t.sg_ospeed = 0;
	t.sg_flags |= ODDP | EVENP | RAW;
	return 0;
    }
    return 1;
}], tcl_cv_api_serial=sgtty, tcl_cv_api_serial=no, tcl_cv_api_serial=no)
    fi
    if test $tcl_cv_api_serial = no ; then
	AC_TRY_RUN([
#include <termios.h>
#include <errno.h>

int main() {
    struct termios t;
    if (tcgetattr(0, &t) == 0
	|| errno == ENOTTY || errno == ENXIO || errno == EINVAL) {
	cfsetospeed(&t, 0);
	t.c_cflag |= PARENB | PARODD | CSIZE | CSTOPB;
	return 0;
    }
    return 1;
}], tcl_cv_api_serial=termios, tcl_cv_api_serial=no, tcl_cv_api_serial=no)
    fi
    if test $tcl_cv_api_serial = no; then
	AC_TRY_RUN([
#include <termio.h>
#include <errno.h>

int main() {
    struct termio t;
    if (ioctl(0, TCGETA, &t) == 0
	|| errno == ENOTTY || errno == ENXIO || errno == EINVAL) {
	t.c_cflag |= CBAUD | PARENB | PARODD | CSIZE | CSTOPB;
	return 0;
    }
    return 1;
    }], tcl_cv_api_serial=termio, tcl_cv_api_serial=no, tcl_cv_api_serial=no)
    fi
    if test $tcl_cv_api_serial = no; then
	AC_TRY_RUN([
#include <sgtty.h>
#include <errno.h>

int main() {
    struct sgttyb t;
    if (ioctl(0, TIOCGETP, &t) == 0
	|| errno == ENOTTY || errno == ENXIO || errno == EINVAL) {
	t.sg_ospeed = 0;
	t.sg_flags |= ODDP | EVENP | RAW;
	return 0;
    }
    return 1;
}], tcl_cv_api_serial=sgtty, tcl_cv_api_serial=none, tcl_cv_api_serial=none)
    fi])
    case $tcl_cv_api_serial in
	termios) AC_DEFINE(USE_TERMIOS);;
	termio)  AC_DEFINE(USE_TERMIO);;
	sgtty)   AC_DEFINE(USE_SGTTY);;
    esac
])

#--------------------------------------------------------------------
# SC_MISSING_POSIX_HEADERS
#
#	Supply substitutes for missing POSIX header files.  Special
#	notes:
#	    - stdlib.h doesn't define strtol, strtoul, or
#	      strtod insome versions of SunOS
#	    - some versions of string.h don't declare procedures such
#	      as strstr
#
# Arguments:
#	none
#	
# Results:
#
#	Defines some of the following vars:
#		NO_DIRENT_H
#		NO_ERRNO_H
#		NO_VALUES_H
#		HAVE_LIMITS_H or NO_LIMITS_H
#		NO_STDLIB_H
#		NO_STRING_H
#		NO_SYS_WAIT_H
#		NO_DLFCN_H
#		HAVE_UNISTD_H
#		HAVE_SYS_PARAM_H
#
#		HAVE_STRING_H ?
#
#--------------------------------------------------------------------

AC_DEFUN([SC_MISSING_POSIX_HEADERS], [
    AC_CACHE_CHECK([dirent.h], tcl_cv_dirent_h, [
    AC_TRY_LINK([#include <sys/types.h>
#include <dirent.h>], [
#ifndef _POSIX_SOURCE
#   ifdef __Lynx__
	/*
	 * Generate compilation error to make the test fail:  Lynx headers
	 * are only valid if really in the POSIX environment.
	 */

	missing_procedure();
#   endif
#endif
DIR *d;
struct dirent *entryPtr;
char *p;
d = opendir("foobar");
entryPtr = readdir(d);
p = entryPtr->d_name;
closedir(d);
], tcl_cv_dirent_h=yes, tcl_cv_dirent_h=no)])

    if test $tcl_cv_dirent_h = no; then
	AC_DEFINE(NO_DIRENT_H)
    fi

    AC_CHECK_HEADER(errno.h, , [AC_DEFINE(NO_ERRNO_H)])
    AC_CHECK_HEADER(float.h, , [AC_DEFINE(NO_FLOAT_H)])
    AC_CHECK_HEADER(values.h, , [AC_DEFINE(NO_VALUES_H)])
    AC_CHECK_HEADER(limits.h,
	[AC_DEFINE(HAVE_LIMITS_H)], [AC_DEFINE(NO_LIMITS_H)])
    AC_CHECK_HEADER(stdlib.h, tcl_ok=1, tcl_ok=0)
    AC_EGREP_HEADER(strtol, stdlib.h, , tcl_ok=0)
    AC_EGREP_HEADER(strtoul, stdlib.h, , tcl_ok=0)
    AC_EGREP_HEADER(strtod, stdlib.h, , tcl_ok=0)
    if test $tcl_ok = 0; then
	AC_DEFINE(NO_STDLIB_H)
    fi
    AC_CHECK_HEADER(string.h, tcl_ok=1, tcl_ok=0)
    AC_EGREP_HEADER(strstr, string.h, , tcl_ok=0)
    AC_EGREP_HEADER(strerror, string.h, , tcl_ok=0)

    # See also memmove check below for a place where NO_STRING_H can be
    # set and why.

    if test $tcl_ok = 0; then
	AC_DEFINE(NO_STRING_H)
    fi

    AC_CHECK_HEADER(sys/wait.h, , [AC_DEFINE(NO_SYS_WAIT_H)])
    AC_CHECK_HEADER(dlfcn.h, , [AC_DEFINE(NO_DLFCN_H)])

    # OS/390 lacks sys/param.h (and doesn't need it, by chance).
    AC_HAVE_HEADERS(unistd.h sys/param.h)
])

#--------------------------------------------------------------------
# SC_PATH_X
#
#	Locate the X11 header files and the X11 library archive.  Try
#	the ac_path_x macro first, but if it doesn't find the X stuff
#	(e.g. because there's no xmkmf program) then check through
#	a list of possible directories.  Under some conditions the
#	autoconf macro will return an include directory that contains
#	no include files, so double-check its result just to be safe.
#
# Arguments:
#	none
#	
# Results:
#
#	Sets the the following vars:
#		XINCLUDES
#		XLIBSW
#
#--------------------------------------------------------------------

AC_DEFUN([SC_PATH_X], [
    AC_PATH_X
    not_really_there=""
    if test "$no_x" = ""; then
	if test "$x_includes" = ""; then
	    AC_TRY_CPP([#include <X11/XIntrinsic.h>], , not_really_there="yes")
	else
	    if test ! -r $x_includes/X11/Intrinsic.h; then
		not_really_there="yes"
	    fi
	fi
    fi
    if test "$no_x" = "yes" -o "$not_really_there" = "yes"; then
	AC_MSG_CHECKING([for X11 header files])
	found_xincludes="no"
	AC_TRY_CPP([#include <X11/Intrinsic.h>], found_xincludes="yes", found_xincludes="no")
	if test "$found_xincludes" = "no"; then
	    dirs="/usr/unsupported/include /usr/local/include /usr/X386/include /usr/X11R6/include /usr/X11R5/include /usr/include/X11R5 /usr/include/X11R4 /usr/openwin/include /usr/X11/include /usr/sww/include"
	    for i in $dirs ; do
		if test -r $i/X11/Intrinsic.h; then
		    AC_MSG_RESULT([$i])
		    XINCLUDES=" -I$i"
		    found_xincludes="yes"
		    break
		fi
	    done
	fi
    else
	if test "$x_includes" != ""; then
	    XINCLUDES="-I$x_includes"
	    found_xincludes="yes"
	fi
    fi
    if test found_xincludes = "no"; then
	AC_MSG_RESULT([couldn't find any!])
    fi

    if test "$no_x" = yes; then
	AC_MSG_CHECKING([for X11 libraries])
	XLIBSW=nope
	dirs="/usr/unsupported/lib /usr/local/lib /usr/X386/lib /usr/X11R6/lib /usr/X11R5/lib /usr/lib/X11R5 /usr/lib/X11R4 /usr/openwin/lib /usr/X11/lib /usr/sww/X11/lib"
	for i in $dirs ; do
	    if test -r $i/libX11.a -o -r $i/libX11.so -o -r $i/libX11.sl; then
		AC_MSG_RESULT([$i])
		XLIBSW="-L$i -lX11"
		x_libraries="$i"
		break
	    fi
	done
    else
	if test "$x_libraries" = ""; then
	    XLIBSW=-lX11
	else
	    XLIBSW="-L$x_libraries -lX11"
	fi
    fi
    if test "$XLIBSW" = nope ; then
	AC_CHECK_LIB(Xwindow, XCreateWindow, XLIBSW=-lXwindow)
    fi
    if test "$XLIBSW" = nope ; then
	AC_MSG_RESULT([could not find any!  Using -lX11.])
	XLIBSW=-lX11
    fi
])

#--------------------------------------------------------------------
# SC_BLOCKING_STYLE
#
#	The statements below check for systems where POSIX-style
#	non-blocking I/O (O_NONBLOCK) doesn't work or is unimplemented. 
#	On these systems (mostly older ones), use the old BSD-style
#	FIONBIO approach instead.
#
# Arguments:
#	none
#	
# Results:
#
#	Defines some of the following vars:
#		HAVE_SYS_IOCTL_H
#		HAVE_SYS_FILIO_H
#		USE_FIONBIO
#		O_NONBLOCK
#
#--------------------------------------------------------------------

AC_DEFUN([SC_BLOCKING_STYLE], [
    AC_CHECK_HEADERS(sys/ioctl.h)
    AC_CHECK_HEADERS(sys/filio.h)
    SC_CONFIG_SYSTEM
    AC_MSG_CHECKING([FIONBIO vs. O_NONBLOCK for nonblocking I/O])
    case $system in
	# There used to be code here to use FIONBIO under AIX.  However, it
	# was reported that FIONBIO doesn't work under AIX 3.2.5.  Since
	# using O_NONBLOCK seems fine under AIX 4.*, I removed the FIONBIO
	# code (JO, 5/31/97).

	OSF*)
	    AC_DEFINE(USE_FIONBIO)
	    AC_MSG_RESULT([FIONBIO])
	    ;;
	SunOS-4*)
	    AC_DEFINE(USE_FIONBIO)
	    AC_MSG_RESULT([FIONBIO])
	    ;;
	ULTRIX-4.*)
	    AC_DEFINE(USE_FIONBIO)
	    AC_MSG_RESULT([FIONBIO])
	    ;;
	*)
	    AC_MSG_RESULT([O_NONBLOCK])
	    ;;
    esac
])

#--------------------------------------------------------------------
# SC_TIME_HANLDER
#
#	Checks how the system deals with time.h, what time structures
#	are used on the system, and what fields the structures have.
#
# Arguments:
#	none
#	
# Results:
#
#	Defines some of the following vars:
#		USE_DELTA_FOR_TZ
#		HAVE_TM_GMTOFF
#		HAVE_TM_TZADJ
#		HAVE_TIMEZONE_VAR
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TIME_HANDLER], [
    AC_CHECK_HEADERS(sys/time.h)
    AC_HEADER_TIME
    AC_STRUCT_TIMEZONE

    AC_CHECK_FUNCS(gmtime_r localtime_r)

    AC_CACHE_CHECK([tm_tzadj in struct tm], tcl_cv_member_tm_tzadj, [
	AC_TRY_COMPILE([#include <time.h>], [struct tm tm; tm.tm_tzadj;],
	    tcl_cv_member_tm_tzadj=yes, tcl_cv_member_tm_tzadj=no)])
    if test $tcl_cv_member_tm_tzadj = yes ; then
	AC_DEFINE(HAVE_TM_TZADJ)
    fi

    AC_CACHE_CHECK([tm_gmtoff in struct tm], tcl_cv_member_tm_gmtoff, [
	AC_TRY_COMPILE([#include <time.h>], [struct tm tm; tm.tm_gmtoff;],
	    tcl_cv_member_tm_gmtoff=yes, tcl_cv_member_tm_gmtoff=no)])
    if test $tcl_cv_member_tm_gmtoff = yes ; then
	AC_DEFINE(HAVE_TM_GMTOFF)
    fi

    #
    # Its important to include time.h in this check, as some systems
    # (like convex) have timezone functions, etc.
    #
    AC_CACHE_CHECK([long timezone variable], tcl_cv_timezone_long, [
	AC_TRY_COMPILE([#include <time.h>],
	    [extern long timezone;
	    timezone += 1;
	    exit (0);],
	    tcl_cv_timezone_long=yes, tcl_cv_timezone_long=no)])
    if test $tcl_cv_timezone_long = yes ; then
	AC_DEFINE(HAVE_TIMEZONE_VAR)
    else
	#
	# On some systems (eg IRIX 6.2), timezone is a time_t and not a long.
	#
	AC_CACHE_CHECK([time_t timezone variable], tcl_cv_timezone_time, [
	    AC_TRY_COMPILE([#include <time.h>],
		[extern time_t timezone;
		timezone += 1;
		exit (0);],
		tcl_cv_timezone_time=yes, tcl_cv_timezone_time=no)])
	if test $tcl_cv_timezone_time = yes ; then
	    AC_DEFINE(HAVE_TIMEZONE_VAR)
	fi
    fi
])

#--------------------------------------------------------------------
# SC_BUGGY_STRTOD
#
#	Under Solaris 2.4, strtod returns the wrong value for the
#	terminating character under some conditions.  Check for this
#	and if the problem exists use a substitute procedure
#	"fixstrtod" (provided by Tcl) that corrects the error.
#	Also, on Compaq's Tru64 Unix 5.0,
#	strtod(" ") returns 0.0 instead of a failure to convert.
#
# Arguments:
#	none
#	
# Results:
#
#	Might defines some of the following vars:
#		strtod (=fixstrtod)
#
#--------------------------------------------------------------------

AC_DEFUN([SC_BUGGY_STRTOD], [
    AC_CHECK_FUNC(strtod, tcl_strtod=1, tcl_strtod=0)
    if test "$tcl_strtod" = 1; then
	AC_CACHE_CHECK([for Solaris2.4/Tru64 strtod bugs], tcl_cv_strtod_buggy,[
	    AC_TRY_RUN([
		extern double strtod();
		int main() {
		    char *infString="Inf", *nanString="NaN", *spaceString=" ";
		    char *term;
		    double value;
		    value = strtod(infString, &term);
		    if ((term != infString) && (term[-1] == 0)) {
			exit(1);
		    }
		    value = strtod(nanString, &term);
		    if ((term != nanString) && (term[-1] == 0)) {
			exit(1);
		    }
		    value = strtod(spaceString, &term);
		    if (term == (spaceString+1)) {
			exit(1);
		    }
		    exit(0);
		}], tcl_cv_strtod_buggy=ok, tcl_cv_strtod_buggy=buggy,
		    tcl_cv_strtod_buggy=buggy)])
	if test "$tcl_cv_strtod_buggy" = buggy; then
	    LIBOBJS="$LIBOBJS fixstrtod.o"
	    AC_DEFINE(strtod, fixstrtod)
	fi
    fi
])

#--------------------------------------------------------------------
# SC_TCL_LINK_LIBS
#
#	Search for the libraries needed to link the Tcl shell.
#	Things like the math library (-lm) and socket stuff (-lsocket vs.
#	-lnsl) are dealt with here.
#
# Arguments:
#	Requires the following vars to be set in the Makefile:
#		DL_LIBS
#		LIBS
#		MATH_LIBS
#	
# Results:
#
#	Subst's the following var:
#		TCL_LIBS
#		MATH_LIBS
#
#	Might append to the following vars:
#		LIBS
#
#	Might define the following vars:
#		HAVE_NET_ERRNO_H
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_LINK_LIBS], [
    #--------------------------------------------------------------------
    # On a few very rare systems, all of the libm.a stuff is
    # already in libc.a.  Set compiler flags accordingly.
    # Also, Linux requires the "ieee" library for math to work
    # right (and it must appear before "-lm").
    #--------------------------------------------------------------------

    AC_CHECK_FUNC(sin, MATH_LIBS="", MATH_LIBS="-lm")
    AC_CHECK_LIB(ieee, main, [MATH_LIBS="-lieee $MATH_LIBS"])

    #--------------------------------------------------------------------
    # Interactive UNIX requires -linet instead of -lsocket, plus it
    # needs net/errno.h to define the socket-related error codes.
    #--------------------------------------------------------------------

    AC_CHECK_LIB(inet, main, [LIBS="$LIBS -linet"])
    AC_CHECK_HEADER(net/errno.h, [AC_DEFINE(HAVE_NET_ERRNO_H)])

    #--------------------------------------------------------------------
    #	Check for the existence of the -lsocket and -lnsl libraries.
    #	The order here is important, so that they end up in the right
    #	order in the command line generated by make.  Here are some
    #	special considerations:
    #	1. Use "connect" and "accept" to check for -lsocket, and
    #	   "gethostbyname" to check for -lnsl.
    #	2. Use each function name only once:  can't redo a check because
    #	   autoconf caches the results of the last check and won't redo it.
    #	3. Use -lnsl and -lsocket only if they supply procedures that
    #	   aren't already present in the normal libraries.  This is because
    #	   IRIX 5.2 has libraries, but they aren't needed and they're
    #	   bogus:  they goof up name resolution if used.
    #	4. On some SVR4 systems, can't use -lsocket without -lnsl too.
    #	   To get around this problem, check for both libraries together
    #	   if -lsocket doesn't work by itself.
    #--------------------------------------------------------------------

    tcl_checkBoth=0
    AC_CHECK_FUNC(connect, tcl_checkSocket=0, tcl_checkSocket=1)
    if test "$tcl_checkSocket" = 1; then
	AC_CHECK_FUNC(setsockopt, , [AC_CHECK_LIB(socket, setsockopt,
	    LIBS="$LIBS -lsocket", tcl_checkBoth=1)])
    fi
    if test "$tcl_checkBoth" = 1; then
	tk_oldLibs=$LIBS
	LIBS="$LIBS -lsocket -lnsl"
	AC_CHECK_FUNC(accept, tcl_checkNsl=0, [LIBS=$tk_oldLibs])
    fi
    AC_CHECK_FUNC(gethostbyname, , [AC_CHECK_LIB(nsl, gethostbyname,
	    [LIBS="$LIBS -lnsl"])])
    
    # Don't perform the eval of the libraries here because DL_LIBS
    # won't be set until we call SC_CONFIG_CFLAGS

    TCL_LIBS='${DL_LIBS} ${LIBS} ${MATH_LIBS}'
    AC_SUBST(TCL_LIBS)
    AC_SUBST(MATH_LIBS)
])

#--------------------------------------------------------------------
# SC_TCL_EARLY_FLAGS
#
#	Check for what flags are needed to be passed so the correct OS
#	features are available.
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		_ISOC99_SOURCE
#		_LARGEFILE64_SOURCE
#		_LARGEFILE_SOURCE64
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_EARLY_FLAG],[
    AC_CACHE_VAL([tcl_cv_flag_]translit($1,[A-Z],[a-z]),
	AC_TRY_COMPILE([$2], $3, [tcl_cv_flag_]translit($1,[A-Z],[a-z])=no,
	    AC_TRY_COMPILE([[#define ]$1[ 1
]$2], $3,
		[tcl_cv_flag_]translit($1,[A-Z],[a-z])=yes,
		[tcl_cv_flag_]translit($1,[A-Z],[a-z])=no)))
    if test ["x${tcl_cv_flag_]translit($1,[A-Z],[a-z])[}" = "xyes"] ; then
	AC_DEFINE($1)
	tcl_flags="$tcl_flags $1"
    fi
])

AC_DEFUN([SC_TCL_EARLY_FLAGS],[
    AC_MSG_CHECKING([for required early compiler flags])
    tcl_flags=""
    SC_TCL_EARLY_FLAG(_ISOC99_SOURCE,[#include <stdlib.h>],
	[char *p = (char *)strtoll; char *q = (char *)strtoull;])
    SC_TCL_EARLY_FLAG(_LARGEFILE64_SOURCE,[#include <sys/stat.h>],
	[struct stat64 buf; int i = stat64("/", &buf);])
    SC_TCL_EARLY_FLAG(_LARGEFILE_SOURCE64,[#include <sys/stat.h>],
	[char *p = (char *)open64;])
    if test "x${tcl_flags}" = "x" ; then
	AC_MSG_RESULT([none])
    else
	AC_MSG_RESULT([${tcl_flags}])
    fi
])

#--------------------------------------------------------------------
# SC_TCL_64BIT_FLAGS
#
#	Check for what is defined in the way of 64-bit features.
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		TCL_WIDE_INT_IS_LONG
#		TCL_WIDE_INT_TYPE
#		HAVE_STRUCT_DIRENT64
#		HAVE_STRUCT_STAT64
#		HAVE_TYPE_OFF64_T
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_64BIT_FLAGS], [
    AC_MSG_CHECKING([for 64-bit integer type])
    AC_CACHE_VAL(tcl_cv_type_64bit,[
	tcl_cv_type_64bit=none
	# See if the compiler knows natively about __int64
	AC_TRY_COMPILE(,[__int64 value = (__int64) 0;],
	    tcl_type_64bit=__int64, tcl_type_64bit="long long")
	# See if we should use long anyway  Note that we substitute in the
	# type that is our current guess for a 64-bit type inside this check
	# program, so it should be modified only carefully...
        AC_TRY_COMPILE(,[switch (0) { 
            case 1: case (sizeof(]${tcl_type_64bit}[)==sizeof(long)): ; 
        }],tcl_cv_type_64bit=${tcl_type_64bit})])
    if test "${tcl_cv_type_64bit}" = none ; then
	AC_DEFINE(TCL_WIDE_INT_IS_LONG)
	AC_MSG_RESULT([using long])
    else
	AC_DEFINE_UNQUOTED(TCL_WIDE_INT_TYPE,${tcl_cv_type_64bit})
	AC_MSG_RESULT([${tcl_cv_type_64bit}])

	# Now check for auxiliary declarations
	AC_CACHE_CHECK([for struct dirent64], tcl_cv_struct_dirent64,[
	    AC_TRY_COMPILE([#include <sys/types.h>
#include <sys/dirent.h>],[struct dirent64 p;],
		tcl_cv_struct_dirent64=yes,tcl_cv_struct_dirent64=no)])
	if test "x${tcl_cv_struct_dirent64}" = "xyes" ; then
	    AC_DEFINE(HAVE_STRUCT_DIRENT64)
	fi

	AC_CACHE_CHECK([for struct stat64], tcl_cv_struct_stat64,[
	    AC_TRY_COMPILE([#include <sys/stat.h>],[struct stat64 p;
],
		tcl_cv_struct_stat64=yes,tcl_cv_struct_stat64=no)])
	if test "x${tcl_cv_struct_stat64}" = "xyes" ; then
	    AC_DEFINE(HAVE_STRUCT_STAT64)
	fi

	AC_CHECK_FUNCS(open64 lseek64)
	AC_MSG_CHECKING([for off64_t])
	AC_CACHE_VAL(tcl_cv_type_off64_t,[
	    AC_TRY_COMPILE([#include <sys/types.h>],[off64_t offset;
],
		tcl_cv_type_off64_t=yes,tcl_cv_type_off64_t=no)])
	dnl Define HAVE_TYPE_OFF64_T only when the off64_t type and the
	dnl functions lseek64 and open64 are defined.
	if test "x${tcl_cv_type_off64_t}" = "xyes" && \
	        test "x${ac_cv_func_lseek64}" = "xyes" && \
	        test "x${ac_cv_func_open64}" = "xyes" ; then
	    AC_DEFINE(HAVE_TYPE_OFF64_T)
	    AC_MSG_RESULT([yes])
	else
	    AC_MSG_RESULT([no])
	fi
    fi
])

#--------------------------------------------------------------------
# SC_TCL_GETHOSTBYADDR_R
#
#	Check if we have MT-safe variant of gethostbyaddr().
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		HAVE_GETHOSTBYADDR_R
#		HAVE_GETHOSTBYADDR_R_7
#		HAVE_GETHOSTBYADDR_R_8
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_GETHOSTBYADDR_R], [AC_CHECK_FUNC(gethostbyaddr_r, [
    AC_CACHE_CHECK([for gethostbyaddr_r with 7 args], tcl_cv_api_gethostbyaddr_r_7, [
    AC_TRY_COMPILE([
	#include <netdb.h>
    ], [
	char *addr;
	int length;
	int type;
	struct hostent *result;
	char buffer[2048];
	int buflen = 2048;
	int h_errnop;

	(void) gethostbyaddr_r(addr, length, type, result, buffer, buflen,
			       &h_errnop);
    ], tcl_cv_api_gethostbyaddr_r_7=yes, tcl_cv_api_gethostbyaddr_r_7=no)])
    tcl_ok=$tcl_cv_api_gethostbyaddr_r_7
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETHOSTBYADDR_R_7)
    else
	AC_CACHE_CHECK([for gethostbyaddr_r with 8 args], tcl_cv_api_gethostbyaddr_r_8, [
	AC_TRY_COMPILE([
	    #include <netdb.h>
	], [
	    char *addr;
	    int length;
	    int type;
	    struct hostent *result, *resultp;
	    char buffer[2048];
	    int buflen = 2048;
	    int h_errnop;

	    (void) gethostbyaddr_r(addr, length, type, result, buffer, buflen,
				   &resultp, &h_errnop);
	], tcl_cv_api_gethostbyaddr_r_8=yes, tcl_cv_api_gethostbyaddr_r_8=no)])
	tcl_ok=$tcl_cv_api_gethostbyaddr_r_8
	if test "$tcl_ok" = yes; then
	    AC_DEFINE(HAVE_GETHOSTBYADDR_R_8)
	fi
    fi
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETHOSTBYADDR_R)
    fi
])])

#--------------------------------------------------------------------
# SC_TCL_GETHOSTBYNAME_R
#
#	Check to see what variant of gethostbyname_r() we have.
#	Based on David Arnold's example from the comp.programming.threads
#	FAQ Q213
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		HAVE_GETHOSTBYADDR_R
#		HAVE_GETHOSTBYADDR_R_3
#		HAVE_GETHOSTBYADDR_R_5
#		HAVE_GETHOSTBYADDR_R_6
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_GETHOSTBYNAME_R], [AC_CHECK_FUNC(gethostbyname_r, [
    AC_CACHE_CHECK([for gethostbyname_r with 6 args], tcl_cv_api_gethostbyname_r_6, [
    AC_TRY_COMPILE([
	#include <netdb.h>
    ], [
	char *name;
	struct hostent *he, *res;
	char buffer[2048];
	int buflen = 2048;
	int h_errnop;

	(void) gethostbyname_r(name, he, buffer, buflen, &res, &h_errnop);
    ], tcl_cv_api_gethostbyname_r_6=yes, tcl_cv_api_gethostbyname_r_6=no)])
    tcl_ok=$tcl_cv_api_gethostbyname_r_6
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETHOSTBYNAME_R_6)
    else
	AC_CACHE_CHECK([for gethostbyname_r with 5 args], tcl_cv_api_gethostbyname_r_5, [
	AC_TRY_COMPILE([
	    #include <netdb.h>
	], [
	    char *name;
	    struct hostent *he;
	    char buffer[2048];
	    int buflen = 2048;
	    int h_errnop;

	    (void) gethostbyname_r(name, he, buffer, buflen, &h_errnop);
	], tcl_cv_api_gethostbyname_r_5=yes, tcl_cv_api_gethostbyname_r_5=no)])
	tcl_ok=$tcl_cv_api_gethostbyname_r_5
	if test "$tcl_ok" = yes; then
	    AC_DEFINE(HAVE_GETHOSTBYNAME_R_5)
	else
	    AC_CACHE_CHECK([for gethostbyname_r with 3 args], tcl_cv_api_gethostbyname_r_3, [
	    AC_TRY_COMPILE([
		#include <netdb.h>
	    ], [
		char *name;
		struct hostent *he;
		struct hostent_data data;

		(void) gethostbyname_r(name, he, &data);
	    ], tcl_cv_api_gethostbyname_r_3=yes, tcl_cv_api_gethostbyname_r_3=no)])
	    tcl_ok=$tcl_cv_api_gethostbyname_r_3
	    if test "$tcl_ok" = yes; then
		AC_DEFINE(HAVE_GETHOSTBYNAME_R_3)
	    fi
	fi
    fi
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETHOSTBYNAME_R)
    fi
])])

#--------------------------------------------------------------------
# SC_TCL_GETPWUID_R
#
#	Check if we have MT-safe variant of getpwuid() and if yes,
#	which one exactly.
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		HAVE_GETPWUID_R
#		HAVE_GETPWUID_R_4
#		HAVE_GETPWUID_R_5
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_GETPWUID_R], [AC_CHECK_FUNC(getpwuid_r, [
    AC_CACHE_CHECK([for getpwuid_r with 5 args], tcl_cv_api_getpwuid_r_5, [
    AC_TRY_COMPILE([
	#include <sys/types.h>
	#include <pwd.h>
    ], [
	uid_t uid;
	struct passwd pw, *pwp;
	char buf[512];
	int buflen = 512;

	(void) getpwuid_r(uid, &pw, buf, buflen, &pwp);
    ], tcl_cv_api_getpwuid_r_5=yes, tcl_cv_api_getpwuid_r_5=no)])
    tcl_ok=$tcl_cv_api_getpwuid_r_5
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETPWUID_R_5)
    else
	AC_CACHE_CHECK([for getpwuid_r with 4 args], tcl_cv_api_getpwuid_r_4, [
	AC_TRY_COMPILE([
	    #include <sys/types.h>
	    #include <pwd.h>
	], [
	    uid_t uid;
	    struct passwd pw;
	    char buf[512];
	    int buflen = 512;

	    (void)getpwnam_r(uid, &pw, buf, buflen);
	], tcl_cv_api_getpwuid_r_4=yes, tcl_cv_api_getpwuid_r_4=no)])
	tcl_ok=$tcl_cv_api_getpwuid_r_4
	if test "$tcl_ok" = yes; then
	    AC_DEFINE(HAVE_GETPWUID_R_4)
	fi
    fi
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETPWUID_R)
    fi
])])

#--------------------------------------------------------------------
# SC_TCL_GETPWNAM_R
#
#	Check if we have MT-safe variant of getpwnam() and if yes,
#	which one exactly.
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		HAVE_GETPWNAM_R
#		HAVE_GETPWNAM_R_4
#		HAVE_GETPWNAM_R_5
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_GETPWNAM_R], [AC_CHECK_FUNC(getpwnam_r, [
    AC_CACHE_CHECK([for getpwnam_r with 5 args], tcl_cv_api_getpwnam_r_5, [
    AC_TRY_COMPILE([
	#include <sys/types.h>
	#include <pwd.h>
    ], [
	char *name;
	struct passwd pw, *pwp;
	char buf[512];
	int buflen = 512;

	(void) getpwnam_r(name, &pw, buf, buflen, &pwp);
    ], tcl_cv_api_getpwnam_r_5=yes, tcl_cv_api_getpwnam_r_5=no)])
    tcl_ok=$tcl_cv_api_getpwnam_r_5
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETPWNAM_R_5)
    else
	AC_CACHE_CHECK([for getpwnam_r with 4 args], tcl_cv_api_getpwnam_r_4, [
	AC_TRY_COMPILE([
	    #include <sys/types.h>
	    #include <pwd.h>
	], [
	    char *name;
	    struct passwd pw;
	    char buf[512];
	    int buflen = 512;

	    (void)getpwnam_r(name, &pw, buf, buflen);
	], tcl_cv_api_getpwnam_r_4=yes, tcl_cv_api_getpwnam_r_4=no)])
	tcl_ok=$tcl_cv_api_getpwnam_r_4
	if test "$tcl_ok" = yes; then
	    AC_DEFINE(HAVE_GETPWNAM_R_4)
	fi
    fi
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETPWNAM_R)
    fi
])])

#--------------------------------------------------------------------
# SC_TCL_GETGRGID_R
#
#	Check if we have MT-safe variant of getgrgid() and if yes,
#	which one exactly.
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		HAVE_GETGRGID_R
#		HAVE_GETGRGID_R_4
#		HAVE_GETGRGID_R_5
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_GETGRGID_R], [AC_CHECK_FUNC(getgrgid_r, [
    AC_CACHE_CHECK([for getgrgid_r with 5 args], tcl_cv_api_getgrgid_r_5, [
    AC_TRY_COMPILE([
	#include <sys/types.h>
	#include <grp.h>
    ], [
	gid_t gid;
	struct group gr, *grp;
	char buf[512];
	int buflen = 512;

	(void) getgrgid_r(gid, &gr, buf, buflen, &grp);
    ], tcl_cv_api_getgrgid_r_5=yes, tcl_cv_api_getgrgid_r_5=no)])
    tcl_ok=$tcl_cv_api_getgrgid_r_5
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETGRGID_R_5)
    else
	AC_CACHE_CHECK([for getgrgid_r with 4 args], tcl_cv_api_getgrgid_r_4, [
	AC_TRY_COMPILE([
	    #include <sys/types.h>
	    #include <grp.h>
	], [
	    gid_t gid;
	    struct group gr;
	    char buf[512];
	    int buflen = 512;

	    (void)getgrgid_r(gid, &gr, buf, buflen);
	], tcl_cv_api_getgrgid_r_4=yes, tcl_cv_api_getgrgid_r_4=no)])
	tcl_ok=$tcl_cv_api_getgrgid_r_4
	if test "$tcl_ok" = yes; then
	    AC_DEFINE(HAVE_GETGRGID_R_4)
	fi
    fi
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETGRGID_R)
    fi
])])

#--------------------------------------------------------------------
# SC_TCL_GETGRNAM_R
#
#	Check if we have MT-safe variant of getgrnam() and if yes,
#	which one exactly.
#
# Arguments:
#	None
#	
# Results:
#
#	Might define the following vars:
#		HAVE_GETGRNAM_R
#		HAVE_GETGRNAM_R_4
#		HAVE_GETGRNAM_R_5
#
#--------------------------------------------------------------------

AC_DEFUN([SC_TCL_GETGRNAM_R], [AC_CHECK_FUNC(getgrnam_r, [
    AC_CACHE_CHECK([for getgrnam_r with 5 args], tcl_cv_api_getgrnam_r_5, [
    AC_TRY_COMPILE([
	#include <sys/types.h>
	#include <grp.h>
    ], [
	char *name;
	struct group gr, *grp;
	char buf[512];
	int buflen = 512;

	(void) getgrnam_r(name, &gr, buf, buflen, &grp);
    ], tcl_cv_api_getgrnam_r_5=yes, tcl_cv_api_getgrnam_r_5=no)])
    tcl_ok=$tcl_cv_api_getgrnam_r_5
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETGRNAM_R_5)
    else
	AC_CACHE_CHECK([for getgrnam_r with 4 args], tcl_cv_api_getgrnam_r_4, [
	AC_TRY_COMPILE([
	    #include <sys/types.h>
	    #include <grp.h>
	], [
	    char *name;
	    struct group gr;
	    char buf[512];
	    int buflen = 512;

	    (void)getgrnam_r(name, &gr, buf, buflen);
	], tcl_cv_api_getgrnam_r_4=yes, tcl_cv_api_getgrnam_r_4=no)])
	tcl_ok=$tcl_cv_api_getgrnam_r_4
	if test "$tcl_ok" = yes; then
	    AC_DEFINE(HAVE_GETGRNAM_R_4)
	fi
    fi
    if test "$tcl_ok" = yes; then
	AC_DEFINE(HAVE_GETGRNAM_R)
    fi
])])

#--------------------------------------------------------------------
# SC_CONFIG_COMMANDS_PRE(CMDS)
#
#	Replacement for autoconf 2.5x AC_COMMANDS_PRE:
#		Commands to run right before config.status is
#		created. Accumulates.
#
#	Requires presence of SC_OUTPUT_COMMANDS_PRE at the end
#	of configure.in (right before AC_OUTPUT).
#
#--------------------------------------------------------------------

AC_DEFUN([SC_CONFIG_COMMANDS_PRE], [
    define([SC_OUTPUT_COMMANDS_PRE], defn([SC_OUTPUT_COMMANDS_PRE])[$1
])])
AC_DEFUN([SC_OUTPUT_COMMANDS_PRE])

