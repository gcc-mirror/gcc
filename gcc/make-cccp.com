$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$!	Build the GNU "C" pre-processor on VMS
$!
$!  Note:  to build with DEC's VAX C compiler, uncomment the 2nd CC, CFLAGS,
$!	   and LIBS alternatives, and also execute the following command:
$!	DEFINE SYS SYS$LIBRARY:
$
$!
$!	C compiler
$!
$ CC	=	"gcc"
$! CC	=	"cc"	!uncomment for VAXC
$ BISON	=	"bison"
$ RENAME=	"rename/New_Version"
$ LINK	=	"link"
$ echo	=	"write sys$output"
$!
$!	Compiler options
$!
$ CFLAGS =	"/Debug/noVerbos"
$! CFLAGS =	"/noOpt"	!uncomment for VAXC
$!
$!	Link options
$!
$ LDFLAGS =	"/noMap"
$!
$!	Link libraries
$!
$ LIBS = "gnu_cc:[000000]gcclib.olb/Libr,sys$library:vaxcrtl.olb/Libr"
$! LIBS = "alloca.obj,sys$library:vaxcrtl.olb/Libr"	!uncomment for VAXC
$
$!!!!!!!
$!	Nothing beyond this point should need any local configuration changes.
$!!!!!!!
$
$ if "''p1'" .eqs. "LINK" then goto Link
$ echo " Building the preprocessor."
$ set verify
$ 'CC''CFLAGS' cccp.c
$!'f$verify(0)
$ t1:='f$search("CEXP.C")'
$ if "''t1'" .eqs. "" then goto 10$
$ t1:='f$file_attributes("CEXP.Y","RDT")'
$ t1:='f$cvtime(t1)'
$ t2:='f$file_attributes("CEXP.C","RDT")'
$ t2:='f$cvtime(t2)'
$ if t1 .les. t2 then goto 20$
$10$:
$ set verify
$ 'BISON' cexp.y
$ 'RENAME' cexp_tab.c cexp.c
$!'f$verify(0)
$20$:
$!
$ if f$locate("alloca.obj",f$edit(LIBS,"lowercase")).lt.f$length(LIBS)
$ then
$  set verify
$ 'CC''CFLAGS'/Define="STACK_DIRECTION=(-1)" alloca.c		!#'f$verify(1)
$!'f$verify(0)
$ endif
$!
$ echo " (Ignore any warning about not finding file ""bison.simple"".)"
$ set verify
$ 'CC''CFLAGS' cexp.c
$ 'CC''CFLAGS' version.c
$!'f$verify(0)
$Link:
$ echo " Linking the preprocessor."
$ set verify
$ 'LINK''LDFLAGS'/Exe=gcc-cpp.exe cccp.obj,cexp.obj,version.obj,version.opt/Opt,-
	  'LIBS'
$!'f$verify(0)
$!
$!	Done
$!
$ exit
