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
$ CC	:=	gcc
$! CC	:=	cc	!uncomment for VAXC
$ BISON	:=	bison
$ RENAME :=	rename
$ LINK	:=	link
$!
$!	Compiler options
$!
$ CFLAGS =	"/debug/incl=([],[.config])"
$! CFLAGS =	"/noopt/incl=([],[.config])"	!uncomment for VAXC
$!
$!	Link options
$!
$ LDFLAGS :=	/nomap
$!
$!	Link libraries
$!
$ LIBS :=	gnu_cc:[000000]gcclib.olb/libr,sys$library:vaxcrtl.olb/libr
$! LIBS :=	alloca.obj,sys$library:vaxcrtl.olb/libr	!uncomment for VAXC
$
$ if "''p1'" .eqs. "LINK" then goto Link
$ 'CC 'CFLAGS cccp.c
$ t1:='f$search("CEXP.C")'
$ if "''t1'" .eqs. "" then goto 10$
$ t1:='f$file_attributes("CEXP.Y","RDT")'
$ t1:='f$cvtime(t1)'
$ t2:='f$file_attributes("CEXP.C","RDT")'
$ t2:='f$cvtime(t2)'
$ if t1 .les. t2 then goto 20$
$ 10$:
$ bison cexp.y
$ rename cexp_tab.c cexp.c
$ 20$:
$!
$ if f$locate("alloca.obj",f$edit(LIBS,"lowercase")).lt.f$length(LIBS) then -
  'CC 'CFLAGS /define="STACK_DIRECTION=(-1)" alloca.c
$!
$ 'CC 'CFLAGS cexp.c
$ 'CC 'CFLAGS version.c
$ Link:
$ link 'LDFLAGS /exe=gcc-cpp cccp,cexp,version,version.opt/opt,'LIBS'
$!
$!	Done
$!
$ exit
