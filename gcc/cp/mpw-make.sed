# Sed commands to finish translating the G++ Unix makefile into MPW syntax.

# Remove control-Ls, they upset MPW make.
s///g

# Remove references to always-empty variables used to mark things.
/CYGNUS-LOCAL-/s/{CYGNUS-LOCAL-[a-z0-9]*}//g

# Add a bunch of definitions, mostly empty.
/^# Variables that exist for you to override.$/a\
\
xmake_file = \
tmake_file = \
build_xm_file = \
MALLOC = \
MD_DEPS = \
REAL_H = \
HOST_CC_LD = {CC_LD}\
ALL_CCLDFLAGS = \
HOST_CCLDFLAGS = \
CONFIG_H = \
LIBDEPS = \

# The "target" variable is special to MPW make, avoid it.
/{target}/s/{target}/{target_canonical}/g

# Suppress the suppression of smart makes.
/^\.y\.c/d

# Whack out "..." assignments.
/\.\.\./s/^\([a-z_]*= \.\.\.\)/#\1/

# Previous edits go a little overboard, undo.
/^objext = /s/"{o}"//

# Always link in low-level MPW functions.
/^LIBDEPS=/s/$/ ::strerror.c.o ::mpwlib.c.o/
/{CLIB}/s/{CLIB}/ ::strerror.c.o ::mpwlib.c.o {CLIB}/

# Don't get tricky about finding various .o file, point at dir above.
/^SUBDIR_OBSTACK/s/`.*`/::obstack.c.o/
/^SUBDIR_USE_ALLOCA/s/`.*`/::alloca.c.o/
/^SUBDIR_MALLOC/s/`.*`//

# Point includes at parent directly correctly.
/^INCLUDES = /s/:\./::/g
/^INCLUDES = /s/"{srcdir}"\.\./"{topsrcdir}"gcc:/g
/^INCLUDES = /s,"{srcdir}"/\.\.,"{topsrcdir}"gcc:,g
/^INCLUDES = /s,"{srcdir}":config,"{topsrcdir}"gcc:config:,g

# Add the special MPW include dirs.
/^INCLUDES = /s/$/ -i "{topsrcdir}"include:mpw: -i :::extra-include:/

# A nasty hack to reduce confusion.
/true/s/ ; @true$//

# (should be in common translation?)
/{CC_LD} /s/$/ {EXTRALIBS}/

# Don't use general compiler flags (which may include definitions
# and other compiler-only bits) with linking commands.
/{CC_LD} /s/ALL_CFLAGS/ALL_CCLDFLAGS/

# Whack out build rules that are not useful.
/^Makefile \\Option-f /,/^$/d
/^config.status \\Option-f /,/^$/d
# (Note that MPW make is not case sensitive, and so this name
# is considered the same as "md_file".)
/^{MD_FILE} \\Option-f/,/^$/d

# Depending on config.status is not useful for us.
/config.status/s/ config.status//

# Repeat of stuff from generic edit.
/{s}/s/"{s}""{s}"/"{s}"/g
/{s}/s/"{s}""{srcdir}"/"{s}"/g
/{s}/s/"{srcdir}""{s}"/"{s}"/g

# Fix references to C frontend files in main dir.
/::c-/s/"{o}"::c-/"{o}":c-/g

# Fix pathnames to generated files in the objdir.
/parse/s/"{s}"parse\.\([chy]\)/"{o}"parse.\1/g
/parse/s/^parse\.\([chy]\)/"{o}"parse.\1/
/y.tab.c/s/"{s}"y\.tab\.c/"{o}"y.tab.c/g
/y.tab.c/s/^y\.tab\.c/"{o}"y.tab.c/
/y.tab.h/s/"{s}"y\.tab\.h/"{o}"y.tab.h/g
/y.tab.h/s/^y\.tab\.h/"{o}"y.tab.h/

# Put in the definition of YYEMPTY directly.
/grep/s/grep .* >>/Echo '#define YYEMPTY -1' >>/

# If the dates are wrong, then this tries to run gperf, which we don't
# really want.
/^"{srcdir}"hash.h/,/hash.h$/d

# Sed the object file list instead of using cat (meow).
/cat/s/`cat /`sed -e 's,:,::,g' -e 's,{objext},.o,g' /

# Simplify dependencies of generated parser files.
/^{PARSE_C}/s/^/#/
/^stamp-parse/s/^stamp-parse/{PARSE_C}/

# Fix the compile line for the generated parser.
/{CC} -c/,/echo {PARSE_C}/c\
	{CC} @DASH_C_FLAG@ {ALL_CFLAGS} {ALL_CPPFLAGS} {INCLUDES} {BIG_SWITCHFLAG} "{o}"parse.c -o "{o}"parse.c.o\

# Change all Rez commands to use mac-gcc.r.
/{REZ}/s/"{s}"[-a-zA-Z{}]*\.r/"{topsrcdir}"gcc:mac-gcc.r/

# Remove pathname junk from the container name.
/{REZ}/s/'"'::cc1plus'"'/'"'cc1plus'"'/
