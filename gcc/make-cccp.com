$v='f$verify(0)	!make-cccp.com
$!
$!	Build the GNU C preprocessor on VMS.
$!
$!	Usage:
$!	  $ @make-cccp.com [compiler] [link-only]
$!
$!	where [compiler] is one of "GNUC", "VAXC", "DECC";
$!	default when none specified is "GNUC",
$!	and where [link-only] is "LINK" or omitted.
$!	If both options are specified, the compiler must come first.
$!
$ if f$type(gcc_debug).eqs."INTEGER" then  if gcc_debug.and.1 then  set verify
$
$ p1 = f$edit(p1,"UPCASE,TRIM")
$ if p1.eqs."" then  p1 = "GNUC"
$!
$!	Compiler-specific setup (assume GNU C, then override as necessary):
$!
$ CC	 = "gcc"
$ CFLAGS = "/Debug/noVerbos"
$ LIBS	 = "gnu_cc:[000000]gcclib.olb/Libr,sys$library:vaxcrtl.olb/Libr"
$ if p1.nes."GNUC"
$ then
$   CC	   = "cc"
$   CFLAGS = "/noOpt"	!disable optimizer when bootstrapping with native cc
$   if p1.eqs."VAXC"
$   then
$     if f$trnlnm("DECC$CC_DEFAULT").nes."" then  CC = "cc/VAXC"
$     LIBS = "alloca.obj,sys$library:vaxcrtl.olb/Libr"
$     define/noLog SYS SYS$LIBRARY:
$   else
$     if p1.eqs."DECC"
$     then
$	if f$trnlnm("DECC$CC_DEFAULT").nes."" then  CC = "cc/DECC"
$	LIBS = "alloca.obj"	!DECC$SHR will be found implicitly by linker
$	define/noLog SYS DECC$LIBRARY_INCLUDE:
$     else
$	if p1.nes."LINK"
$	then
$	  type sys$input: /Output=sys$error:
$DECK
[compiler] argument should be one of "GNUC", "VAXC", or "DECC".

Usage:
$ @make-cccp.com [compiler] [link-only]

$EOD
$	  exit %x1000002C + 0*f$verify(v)	!%SYSTEM-F-ABORT
$	endif !!LINK
$     endif !DECC
$   endif !VAXC
$ endif !!GNUC
$
$!
$!	Other setup:
$!
$ LDFLAGS =	"/noMap"
$ PARSER  =	"bison"
$ RENAME  =	"rename/New_Version"
$ LINK	  =	"link"
$ echo	  =	"write sys$output"
$
$!!!!!!!
$!	Nothing beyond this point should need any local configuration changes.
$!!!!!!!
$
$! Set the default directory to the same place as this command procedure.
$ flnm = f$enviroment("PROCEDURE")	!get current procedure name
$ set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$
$ if p1.eqs."LINK" .or. p2.eqs."LINK" then  goto Link
$ echo " Building the preprocessor."
$
$! Compile the simplest file first, to catch problem with compiler setup early.
$ set verify
$ 'CC''CFLAGS' version.c
$!'f$verify(0)
$
$ set verify
$ 'CC''CFLAGS' cccp.c
$!'f$verify(0)
$
$! Compile preprocessor's parser, possibly making it with yacc first.
$ if f$search("CEXP.C").nes."" then -
    if f$cvtime(f$file_attributes("CEXP.C","RDT")).ges.-
       f$cvtime(f$file_attributes("CEXP.Y","RDT")) then  goto skip_yacc
$ set verify
$ 'PARSER' cexp.y
$ 'RENAME' cexp_tab.c cexp.c
$!'f$verify(0)
$skip_yacc:
$ echo " (Ignore any warning about not finding file ""bison.simple"".)"
$ set verify
$ 'CC''CFLAGS' cexp.c
$!'f$verify(0)
$ 
$! In case there's no builtin alloca support, use the C simulation.
$ if f$locate("alloca.obj",f$edit(LIBS,"lowercase")).lt.f$length(LIBS)
$ then
$  set verify
$ 'CC''CFLAGS'/Incl=[]/Defi=("HAVE_CONFIG_H","STACK_DIRECTION=(-1)") alloca.c
$!'f$verify(0)
$ endif
$!
$
$Link:
$ echo " Linking the preprocessor."
$ set verify
$ 'LINK''LDFLAGS'/Exe=gcc-cpp.exe -
	  cccp.obj,cexp.obj,version.obj,version.opt/Opt,-
	  'LIBS'
$!'f$verify(0)
$!
$!	Done
$!
$ exit 1+0*f$verify(v)
