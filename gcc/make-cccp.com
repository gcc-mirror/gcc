$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$!	Build the GNU "C" pre-processor on VMS
$!
$
$!
$!	C compiler
$!
$ CC	:=	gcc
$ BISON	:=	bison
$ RENAME :=	rename
$ LINK	:=	link
$!
$!	Compiler options
$!
$ CFLAGS =	"/debug/inc=([],[.config])"
$!
$!	Link options
$!
$ LDFLAGS :=	/nomap
$!
$!	Link libraries
$!
$ LIBS :=	gnu_cc:[000000]gcclib/libr,sys$share:vaxcrtl/libr
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
$ 'CC 'CFLAGS cexp.c
$ 'CC 'CFLAGS version.c
$ Link:
$ link 'LDFLAGS /exe=gcc-cpp cccp,cexp,version,version.opt/opt,'LIBS'
$!
$! CAUTION: If you want to link gcc-cpp to the sharable image library
$! VAXCRTL, see the notes in gcc.texinfo (or INSTALL) first.
$!
$!	Done
$!
$ exit
