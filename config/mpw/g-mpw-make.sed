# Sed commands to translate Unix makefiles into MPW makefiles.
# These are nominally generic, but work best on the makefiles used
# for GNU programs.

# Whack out any commented-out lines that are probably commands;
# they can only cause trouble later on.
/^#	/d

# Change dependency char.
/:$/s/:/ \\Option-f/g
/^[^ 	:#][^:]*:/s/\([ 	]*\):\([ 	]*\)/ \\Option-f /g

# Change syntax of Makefile vars.
/\$/s/\${\([a-zA-Z0-9_-]*\)}/{\1}/g
/\$/s/\$(\([a-zA-Z0-9_-]*\))/{\1}/g
/ $@/s/ $@/ {Targ}/

# Double-$ are literals to Unix but not to MPW make.
/\$\$/s/\$\$/$/g

# Change pathname syntax.
/\//s,\.\./\/\.\./,:::,g
/\//s,\.\./,::,g
/\.\//s,\./,:,g
/\//s,/,:,g
# Undo excess changes.
/and/s,and:or$,and/or,
/and/s,and:or ,and/or ,
/want/s,want:need,want/need,
# Fixing up sed commands.
/-e/s_":\([^:]*\):d"_"/\1/d"_g
/-e/s_":\([^:]*\):,:\([^:]*\):d"_"/\1/,/\2/d"_g

/=/s/ = \.$/ = :/

# Make these go away so that later edits not confused.
/HLDENV/s/{HLDENV}//

# Comment out any explicit srcdir setting.
/srcdir/s/^srcdir/# srcdir/

/BASEDIR/s/^BASEDIR =.*$/BASEDIR = "{srcroot}"/
/{BASEDIR}:/s/{BASEDIR}:/{BASEDIR}/g
/{srcdir}:/s/{srcdir}:/"{srcdir}"/g
/"{srcdir}":/s/"{srcdir}":/"{srcdir}"/g

# Tweak some conventions that are backwards for the Mac.
/bindir/s/{exec_prefix}:bin/{exec_prefix}bin:/
/libdir/s/{exec_prefix}:lib/{exec_prefix}lib:/

# Comment out settings of anything set by mpw host config.
/CC/s/^CC *=/#CC =/
/CFLAGS/s/^CFLAGS *=/#CFLAGS =/
/AR/s/^AR *=/#AR =/
/AR_FLAGS/s/^AR_FLAGS *=/#AR_FLAGS =/
/RANLIB/s/^RANLIB *=/#RANLIB =/
/CC_LD/s/^CC_LD *=/#CC_LD =/
/LDFLAGS/s/^LDFLAGS *=/#LDFLAGS =/

# Change -I usages.
/-I/s/-I\./-i :/g
/-I/s/-I::bfd/-i ::bfd:/g
/-I/s/-I::include/-i ::include:/g
/-I/s/-I/-i /g

# Change -D usage.
/-D/s/\([ =]\)-D\([^ ]*\)/\1-d \2/g

# Change continuation char.
/\\$/s/\\$/\\Option-d/

# Change wildcard char.
/\*/s/\*/\\Option-x/g

# Change path of various types of source files.  This rule does not allow
# for file names with multiple dots in the name.
/\.[chly]/s/\([ 	><=]\)\([-a-zA-Z0-9_${}:"]*\)\.\([chly]\)/\1"{s}"\2.\3/g
/\.[chly]/s/^\([-a-zA-Z0-9_${}:"]*\)\.\([chly]\)/"{s}"\1.\2/
# Allow files named *.tab.[ch] as a special case.
/\.tab\.[ch]/s/\([ 	><=]\)\([-a-zA-Z0-9_${}:"]*\.tab\)\.\([ch]\)/\1"{s}"\2.\3/g
/\.tab\.[ch]/s/^\([-a-zA-Z0-9_${}:"]*\.tab\)\.\([ch]\)/"{s}"\1.\2/
# Fix some overenthusiasms.
/{s}/s/"{s}""{srcdir}"/"{srcdir}"/g
/{s}/s/"{s}"{\([a-zA-Z0-9_]*\)dir}/"{\1dir}"/g
/{s}/s/"{s}"{\([a-zA-Z0-9_]*\)DIR}/"{\1DIR}"/g
/{s}/s/"{s}""{\([a-zA-Z0-9_]*\)dir}"/"{\1dir}"/g
/{s}/s/"{s}""{\([a-zA-Z0-9_]*\)DIR}"/"{\1DIR}"/g
/{s}/s/"{s}":/:/g
/{s}/s/^"{s}"//g
/{s}/s/"{s}""{s}"/"{s}"/g
/{s}/s/"{s}""{srcdir}"/"{s}"/g
/{s}/s/"{srcdir}""{s}"/"{s}"/g

# The .def files are also typically source files.
/\.def/s/\([ 	><]\)\([-a-zA-Z0-9_${}:"]*\)\.def/\1"{s}"\2.def/g
/\.def/s/^\([-a-zA-Z0-9_${}:"]*\)\.def/"{s}"\1.def/g

# Change extension and path of objects.
/\.o/s/\([ 	=]\)\([-a-zA-Z0-9_${}:"]*\)\.o/\1"{o}"\2.c.o/g
/\.o/s/^\([-a-zA-Z0-9_${}:"]*\)\.o/"{o}"\1.c.o/
# Allow *.tab.o files as a special case of a 2-dot-name file.
/\.o/s/\([ 	=]\)\([-a-zA-Z0-9_${}:"]*\)\.tab\.o/\1"{o}"\2.tab.c.o/g
/\.o/s/^\([-a-zA-Z0-9_${}:"]*\)\.tab\.o/"{o}"\1.tab.c.o/
# Clean up.
/"{o}"/s/"{o}""{o}"/"{o}"/g
/"{o}"/s/^"{o}"\([a-zA-Z0-9_]*\)=/\1=/

# Change extension of libs.
/\.a/s/lib\([a-z]*\)\.a/lib\1.o/g

# Remove non-fail option.
/-/s/^\([ 	]*\)-/\1/
# Fix overeagernesses - assumes no one-letter commands.
/^[ 	]*[a-z] /s/^\([ 	]*\)\([a-z]\) /\1-\2 /

# Remove non-echo option. (watch out for autoconf things)
/@/s/^\([ 	]*\)@/\1/

# Change cp to Duplicate.
# Catenate is perhaps more accurate, but the pattern would have to
# identify the output file and add a '>' redirection into it.
/cp/s/^\([ 	]*\)cp /\1Duplicate -d -y /
# Change mv to Rename.
/mv/s/^\([ 	]*\)mv /\1Rename -y /
/Rename/s/^\([ 	]*\)Rename -y -f/\1Rename -y/
# Change rm to Delete.
/rm -rf/s/^\([ 	]*\)rm -rf /\1Delete -i -y /
/rm -f/s/^\([ 	]*\)rm -f /\1Delete -i -y /
/rm/s/^\([ 	]*\)rm /\1Delete -i -y /
# Note that we don't mess with ln - directory-specific scripts
# must decide what to do with symlinks.
# Change cat to Catenate.
/cat/s/^\([ 	]*\)cat /\1Catenate /
# Change touch to mpw-touch.
/touch/s/^\([ 	]*\)touch /\1mpw-touch /
# Change mkdir to NewFolder.
/mkdir/s/^\([ 	]*\)mkdir /\1NewFolder /
# Change var setting to Set.
/=/s/^\([ 	]*\)\([-a-zA-Z0-9_]*\)=\([^;]*\); \\Option-d/\1Set \2 \3/

# Change tests.
/if /s/if \[ *-f \([^ ]*\) ] *; *\\Option-d/If "`Exists "\1"`" != ""/
/if /s/if \[ *-f \([^ ]*\) ] *; *then *\\Option-d/If "`Exists "\1"`" != ""/
/if /s/if \[ ! *-f \([^ ]*\) ] *; *\\Option-d/If "`Exists "\1"`" == ""/
/if /s/if \[ ! *-f \([^ ]*\) ] *; *then \\Option-d/If "`Exists "\1"`" == ""/

/if /s/if \[ *-d \([^ ]*\) ] *; *\\Option-d/If "`Exists "\1"`" != ""/
/if /s/if \[ *-d \([^ ]*\) ] *; *then *\\Option-d/If "`Exists "\1"`" != ""/
/if /s/if \[ ! *-d \([^ ]*\) ] *; *\\Option-d/If "`Exists "\1"`" == ""/
/if /s/if \[ ! *-d \([^ ]*\) ] *; *then *\\Option-d/If "`Exists "\1"`" == ""/

/if /s/if \[ -d \([^ ]*\) ] *; then true *; else mkdir \([^ ;]*\) *; fi/If "`Exists "\1"`" != "" NewFolder \2 End If/

/if /s/if \[ \([^ ]*\) = \([^ ]*\) ] *; *\\Option-d/If "\1" == "\2"/
/if /s/if \[ \([^ ]*\) = \([^ ]*\) ] *; *then *\\Option-d/If "\1" == "\2"/

/if /s/if \[ \([^ ]*\) != \([^ ]*\) ] *; *\\Option-d/If "\1" != "\2"/
/if /s/if \[ \([^ ]*\) != \([^ ]*\) ] *; *then *\\Option-d/If "\1" != "\2"/

/if /s/if \[ \([^ ]*\) -eq \([^ ]*\) ] *; *\\Option-d/If "\1" != "\2"/
/if /s/if \[ \([^ ]*\) -eq \([^ ]*\) ] *; *then *\\Option-d/If "\1" != "\2"/

/^[ 	]*else true$/c\
	Else\
		mpw-true\


/else/s/^\([ 	]*\)else[ 	]*$/\1Else/
/else/s/^\([ 	]*\)else[; 	]*\\Option-d$/\1Else/

/^[ 	]*else[ 	]*true[ 	]*$/c\
	Else\
		mpw-true

/^[ 	]*else[ 	]*true[; 	]*fi$/c\
	Else\
		mpw-true\
	End If

/fi/s/^\([ 	]*\)fi *$/\1End/
/fi/s/^\([ 	]*\)fi *; *\\Option-d/\1End/

# Change looping.
/for/s/^\([ 	]*\)for \([-a-zA-Z0-9_]*\) in \([^;]*\); *do *\\Option-d/\1For \2 In \3/
/^\([ 	]*\)do *\\Option-d/d
/done/s/^\([ 	]*\)done *; *\\Option-d/\1End/
/done/s/^\([ 	]*\)done$/\1End/

# Trailing semicolons and continued lines are unneeded sh syntax.
/; \\Option-d/s/; \\Option-d//

# Change move-if-change to MoveIfChange.
/move-if-change/s/\([^ 	]*\)move-if-change/MoveIfChange/g

# Change $(SHELL) to the script name by itself.
/SHELL/s/^\([ 	]*\){SHELL} /\1/

# Change syntax of default rule dependency.
/^\.c\.o/s/^\.c\.o \\Option-f$/.c.o \\Option-f .c/

# Change default rule's action.
/{CC} -c/s/{CC} -c \(.*\) \$<$/{CC} @DASH_C_FLAG@ {DepDir}{Default}.c \1 @SEGMENT_FLAG({Default})@ -o {TargDir}{Default}.c.o/

# This is pretty disgusting, but I can't seem to detect empty rules.
/Option-f$/s/Option-f$/Option-f _oldest/g

# Remove -c from explicit compiler calls. (but should not if GCC)
# Handle the case of a source file that is "{xxx}"file.c.
/ -c /s/{\([A-Z_]*\)CC}\(.*\) -c \(.*\)"\([^"]*\)"\([-a-z_]*\)\.c/{\1CC}\2 @DASH_C_FLAG@ \3"\4"\5.c -o "{o}"\5.c.o/
# Handle the case of a source file that is "{xxx}"dir:file.c.
/ -c /s/{\([A-Z_]*\)CC}\(.*\) -c \(.*\)"\([^"]*\)"\([-a-z_]*\):\([-a-z_]*\)\.c/{\1CC}\2 @DASH_C_FLAG@ \3"\4"\5:\6.c -o "{o}"\6.c.o/

# Change linking cc to linking sequence.
/-o/s/^\([ 	]*\){CC} \(.*\){\([A-Z_]*\)CFLAGS} \(.*\){LDFLAGS} \(.*\)-o \([^ ]*\) \(.*\)$/\1{CC_LD} \2 {\3CFLAGS} \4 {LDFLAGS} \5 -o \6{PROG_EXT} \7\
\1{MAKEPEF} \6{PROG_EXT} -o \6 {MAKEPEF_TOOL_FLAGS} {MAKEPEF_FLAGS}\
\1{REZ} "{s}"\6.r -o \6 -append -d PROG_NAME='"'\6'"' -d VERSION_STRING='"'{version}'"'/
/-o/s/^\([ 	]*\){CC} \(.*\){\([A-Z_]*\)CFLAGS} \(.*\)-o \([^ ]*\) \(.*\){LDFLAGS} \(.*\)$/\1{CC_LD} \2 {\3CFLAGS} \4 {LDFLAGS} \6 -o \5{PROG_EXT} \7\
\1{MAKEPEF} \5{PROG_EXT} -o \5 {MAKEPEF_TOOL_FLAGS} {MAKEPEF_FLAGS}\
\1{REZ} "{s}"\5.r -o \5 -append -d PROG_NAME='"'\5'"' -d VERSION_STRING='"'{version}'"'/
/-o/s/^\([ 	]*\){HOST_CC} \(.*\)-o \([^ ]*\) \(.*\)$/\1{HOST_CC_LD} \2 -o \3{PROG_EXT} \4\
\1{MAKEPEF} \3{PROG_EXT} -o \3 {MAKEPEF_TOOL_FLAGS} {MAKEPEF_FLAGS}\
\1{REZ} "{s}"\3.r -o \3 -append -d PROG_NAME='"'\3'"' -d VERSION_STRING='"'{version}'"'/

# Comment out .NOEXPORT rules.
/\.NOEXPORT/s/^\.NOEXPORT/#\.NOEXPORT/
# Comment out .PHONY rules.
/\.PHONY/s/^\.PHONY/#\.PHONY/
# Comment out .PRECIOUS rules.
/\.PRECIOUS/s/^\.PRECIOUS/#\.PRECIOUS/
# Comment out .SUFFIXES rules.
/\.SUFFIXES/s/^\.SUFFIXES/#\.SUFFIXES/

# Set the install program appropriately.
/INSTALL/s/^INSTALL *= *`.*`:install.sh -c/INSTALL = Duplicate -y/

# Don't try to decide whether to use the tree's own tools.
/bison/s/`.*bison:bison.*`/bison -y/
/byacc/s/`.*byacc:byacc.*`/byacc/
/flex/s/`.*flex:flex.*`/flex/

# Turn transformed C comments in echo commands back into comments.
/echo/s,echo '\(.*\):\\Option-x\(.*\)\\Option-x:\(.*\)',echo '\1/*\2*/\3',

# Whack out various clever expressions that search for tools, since
# the clever code is too /bin/sh specific.

/^AR_FOR_TARGET = `/,/`$/c\
AR_FOR_TARGET = ::binutils:ar\


/^RANLIB_FOR_TARGET = `/,/`$/c\
RANLIB_FOR_TARGET = ::binutils:ranlib\


/^RANLIB_TEST_FOR_TARGET = /,/ranlib ] )$/c\
RANLIB_TEST_FOR_TARGET = \


/^EXPECT = `/,/`$/c\
EXPECT = \


/^RUNTEST = `/,/`$/c\
RUNTEST = \


/^CC_FOR_TARGET = `/,/`$/c\
CC_FOR_TARGET = \


/^CXX_FOR_TARGET = `/,/`$/c\
CXX_FOR_TARGET = \


/^CHILL_FOR_TARGET = `/,/`$/c\
CHILL_FOR_TARGET = \


/^CHILL_LIB = `/,/`$/c\
CHILL_LIB = \

/sanit/s/{start-sanit...-[a-z0-9]*}//
/sanit/s/{end-sanit...-[a-z0-9]*}//

# Add standard defines and default rules.
/^# srcdir/a\
\
s = "{srcdir}"\
\
o = :\
\
"{o}" \\Option-f : "{s}"

