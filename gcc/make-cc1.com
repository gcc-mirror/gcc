$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$ v=f$verify(0)
$!
$! CAUTION: If you want to link gcc-cc1 to the sharable image library
$! VAXCRTL, see the notes in gcc.texinfo (or INSTALL) first.
$!
$!	Build the GNU "C" compiler on VMS
$!
$!  Note:  to build with DEC's VAX C compiler, uncomment the 2nd CC, CFLAGS,
$!	   and LIBS alternatives, and also execute the following command:
$!	DEFINE SYS SYS$LIBRARY:
$!	   After a successful build, restore those items and rebuild with gcc.
$
$!	C compiler
$!
$ CC	=	"gcc"
$! CC	=	"cc"	!uncomment for VAXC
$ BISON	=	"bison"
$ BISON_FLAGS=	"/Define/Verbose"
$ RENAME=	"rename/New_Version"
$ LINK	=	"link"
$ EDIT	=	"edit"
$ SEARCH=	"search"
$ echo	=	"write sys$output"
$!
$!	Compiler options
$!
$ CFLAGS =	"/Debug/noVerbos/CC1=""-mpcc-alignment"""
$! CFLAGS =	"/noOpt"		!uncomment for VAXC
$ CINCL1 =	"/Incl=[]"			!stage 1 -I flags
$ CINCL2 =	"/Incl=([],[.ginclude])"	!stage 2,3,... flags
$ CINCL_SUB =	"/Incl=([],[-],[-.ginclude])"	![.cp] flags
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
$!
$!  First we figure out what needs to be done.  This is sort of like a limited
$! make facility - the command line options specify exactly what components
$! we want to build.  The following options are understood:
$!
$!	LINK:	Assume that the object modules for the selected compiler(s)
$!		have already been compiled, perform link phase only.
$!
$!	CC1:	Compile and link "C" compiler.
$!
$!	CC1PLUS:Compile and link "C++" compiler.
$!
$!	CC1OBJ:	Compile and link objective C compiler.
$!
$!	ALL:	Compile and link all of the CC1 passes.
$!
$!	INDEPENDENT:
$!		Compile language independent source modules. (On by default).
$!
$!	BC:
$!		Compile byte compiler source modules. (On by default).
$!
$!	DEBUG:	Link images with /debug.
$!
$! If you want to list more than one option, you should use a spaces to
$! separate them.
$!
$!	Any one of the above options can be prefaced with a "NO".  For example,
$! if you had already built GCC, and you wanted to build G++, you could use the
$! "CC1PLUS NOINDEPENDENT" options, which would only compile the C++ language
$! specific source files, and then link the C++ compiler.
$!
$! If you do not specify which compiler you want to build, it is assumed that
$! you want to build GNU-C ("CC1").
$!
$! Now figure out what we have been requested to do.
$p1 = p1+" "+p2+" "+p3+" "+p4+" "+p5+" "+p6+" "+p7 
$p1 = f$edit(p1,"COMPRESS")
$i=0
$DO_ALL = 0
$DO_LINK = 0
$DO_DEBUG = 0
$DO_CC1PLUS = 0
$if f$trnlnm("cfile$").nes."" then  close/noLog cfile$
$open cfile$ compilers.list
$cinit:read cfile$ compilername/end=cinit_done
$DO_'compilername'=0
$goto cinit
$cinit_done: close cfile$
$DO_INDEPENDENT = 1
$DO_DEFAULT = 1
$DO_BC = 1
$loop:
$string = f$element(i," ",p1)
$if string.eqs." " then goto done
$flag = 1
$if string.eqs."CC1PLUS" then DO_DEFAULT = 0
$if string.eqs."CC1OBJ" then DO_DEFAULT = 0
$if f$extract(0,2,string).nes."NO" then goto parse_option
$  string=f$extract(2,f$length(string)-2,string)
$  flag = 0
$parse_option:
$DO_'string' = flag
$i=i+1
$goto loop
$!
$done:
$if DO_DEFAULT.eq.1 then DO_CC1 = 1
$echo "This command file will now perform the following actions:
$if DO_LINK.eq.1 then goto link_only
$if DO_ALL.eq.1 then echo "   Compile all language specific object modules."
$if DO_CC1.eq.1 then echo "   Compile C specific object modules."
$if DO_CC1PLUS.eq.1 then echo "   Compile C++ specific object modules."
$if DO_CC1OBJ.eq.1 then echo "   Compile obj-C specific object modules."
$if DO_INDEPENDENT.eq.1 then echo "   Compile language independent object modules."
$if DO_BC.eq.1 then echo "   Compile byte compiler object modules."
$link_only:
$if DO_CC1.eq.1 then	echo "   Link C compiler (gcc-cc1.exe)."
$if DO_CC1PLUS.eq.1 then echo "   Link C++ compiler (gcc-cc1plus.exe)."
$if DO_CC1OBJ.eq.1 then echo "   Link objective-C compiler (gcc-cc1obj.exe)."
$if DO_DEBUG.eq.1 then echo  "   Link images to run under debugger."
$!
$! Update CFLAGS with appropriate CINCLx value.
$!
$if f$edit(f$extract(0,3,CC),"LOWERCASE").nes."gcc" then goto stage1
$if f$search("gcc-cc1.exe").eqs."" then goto stage1
$if f$file_attr("gnu_cc:[000000]gcc-cc1.exe","FID").nes.-
    f$file_attr("gcc-cc1.exe","FID") then goto stage1
$ CFLAGS = CFLAGS + CINCL2
$ goto cinclX
$stage1:
$ CFLAGS = CFLAGS + CINCL1
$cinclX:
$!
$! Test and see if we need these messages or not.  The -1 switch gives it away.
$!
$gas := $gnu_cc:[000000]gcc-as.exe
$if f$search(gas-"$").eqs."" then  goto gas_message	!must be VAXC
$define/user sys$error sys$scratch:gas_test.tmp
$gas -1 nla0: -o nla0:
$size=f$file_attributes("sys$scratch:gas_test.tmp","ALQ")
$delete/nolog sys$scratch:gas_test.tmp;*
$if size.eq.0 then goto no_message
$gas_message:
$type sys$input

	Note: GCC 2.x treats external variables differently than GCC 1.x does.
Before you use GCC 2.x, you should obtain a version of the assembler which
contains the patches to work with GCC 2.x (GCC-AS 1.38 does not contain
these patches - whatever comes after this probably will).  The assembler
in gcc-vms-1.42.tar.gz from prep does contain the proper patches.

	If you do not update the assembler, the compiler will still work,
but `extern const' variables will be treated as `extern'.  This will result
in linker warning messages about mismatched psect attributes, and these
variables will be placed in read/write storage.

$!
$no_message:
$!
$!
$ if DO_DEBUG.eq.1 then LDFLAGS = LDFLAGS + "/Debug"
$!
$if DO_LINK.eq.1 then goto compile_cc1
$!
$! Build alloca if necessary (in 'LIBS for use with VAXC)
$!
$ if f$locate("alloca.obj",f$edit(LIBS,"lowercase")).ge.f$length(LIBS) then -
	goto skip_alloca
$ if f$search("alloca.obj").nes."" then -  !does .obj exist? is it up to date?
    if f$cvtime(f$file_attributes("alloca.obj","RDT")).gts.-
       f$cvtime(f$file_attributes("alloca.c","RDT")) then  goto skip_alloca
$set verify
$ 'CC''CFLAGS'/Define="STACK_DIRECTION=(-1)" alloca.c
$!'f$verify(0)
$skip_alloca:
$!
$if DO_BC.eq.1 
$	THEN 
$	call compile bi_all.opt ""
$	open ifile$ bc_all.opt
$	read ifile$ bc_line
$	close ifile$
$	bc_index = 0
$bc_loop:
$	tfile = f$element(bc_index, " ", bc_line)
$	if tfile.eqs." " then goto bc_done
$	call bc_generate 'tfile' "bi_all.opt/opt,"
$	bc_index = bc_index + 1
$	goto bc_loop
$bc_done:
$	endif
$!
$!
$if DO_INDEPENDENT.eq.1 
$	THEN 
$!
$! First build a couple of header files from the machine description
$! These are used by many of the source modules, so we build them now.
$!
$set verify
$ 'CC''CFLAGS' rtl.c
$ 'CC''CFLAGS' obstack.c
$!'f$verify(0)
$! Generate insn-attr.h
$	call generate insn-attr.h
$	call generate insn-flags.h
$	call generate insn-codes.h
$	call generate insn-config.h
$!
$call compile independent.opt "rtl,obstack,insn-attrtab"
$!
$	call generate insn-attrtab.c "rtlanal.obj,"
$set verify
$ 'CC''CFLAGS' insn-attrtab.c
$ 'CC''CFLAGS' bc-emit.c
$ 'CC''CFLAGS' bc-optab.c
$!'f$verify(0)
$	endif
$!
$compile_cc1:
$if (DO_CC1 + DO_CC1OBJ) .ne.0
$	then
$if (f$search("C-PARSE.Y") .eqs. "") then goto yes_yfiles
$if (f$cvtime(f$file_attributes("C-PARSE.IN","RDT")).gts. -
 	    f$cvtime(f$file_attributes("C-PARSE.Y","RDT")))  -
		then goto yes_yfiles
$if (f$search("OBJC-PARSE.Y") .eqs. "") then goto yes_yfiles
$if (f$cvtime(f$file_attributes("C-PARSE.IN","RDT")).gts. -
 	    f$cvtime(f$file_attributes("OBJC-PARSE.Y","RDT")))  -
		then goto yes_yfiles
$GOTO no_yfiles
$yes_yfiles:
$echo "Now processing c-parse.in to generate c-parse.y and objc-parse.y."
$ edit/tpu/nojournal/nosection/nodisplay/command=sys$input
!
!     Read c-parse.in, write c-parse.y and objc-parse.y, depending on
!     paired lines of "ifc" & "end ifc" and "ifobjc" & "end ifobjc" to
!     control what goes into each file.  Most lines will be common to
!     both (hence not bracketed by either control pair).  Mismatched
!     pairs aren't detected--garbage in, garbage out...
!

   PROCEDURE do_output()
      IF NOT objc_only THEN POSITION(END_OF(c)); COPY_TEXT(input_line); ENDIF;
      IF NOT c_only THEN POSITION(END_OF(objc)); COPY_TEXT(input_line); ENDIF;
      POSITION(input_file);                     !reset
   ENDPROCEDURE;

   input_file := CREATE_BUFFER("input", "c-parse.in");  !load data
		 SET(NO_WRITE, input_file);
   c          := CREATE_BUFFER("c_output");     !1st output file
   objc       := CREATE_BUFFER("objc_output");  !2nd output file

   POSITION(BEGINNING_OF(input_file));
   c_only     := 0;
   objc_only  := 0;

   LOOP
      EXITIF MARK(NONE) = END_OF(input_file);   !are we done yet?

      input_line := CURRENT_LINE;               !access current_line just once
      CASE EDIT(input_line, TRIM_TRAILING, OFF, NOT_IN_PLACE)
	 ["ifc"]        : c_only := 1;
	 ["end ifc"]    : c_only := 0;
	 ["ifobjc"]     : objc_only := 1;
	 ["end ifobjc"] : objc_only := 0;
!         default -- add non-control line to either or both output files
	 [INRANGE]      : do_output();          !between "end" and "if"
	 [OUTRANGE]     : do_output();          !before "end" or after "if"
      ENDCASE;

      MOVE_VERTICAL(1);                         !go to next line
   ENDLOOP;

   WRITE_FILE(c, "c-parse.y");
   WRITE_FILE(objc, "objc-parse.y");
   QUIT
$	endif	
$no_yfiles:
$!
$open cfile$ compilers.list
$cloop:read cfile$ compilername/end=cdone
$! language specific modules
$!
$if (DO_ALL + DO_'compilername').eq.0 then goto cloop
$if DO_LINK.eq.0 then -
 call compile 'compilername'-objs.opt "obstack,bc-emit,bc-optab"
$!
$! CAUTION: If you want to link gcc-cc1* to the sharable image library
$! VAXCRTL, see the notes in gcc.texinfo (or INSTALL) first.
$!
$set verify
$ 'LINK''LDFLAGS'/Exe=gcc-'compilername'.exe  version.opt/Opt,-
	  'compilername'-objs.opt/Opt,independent.opt/Opt,-
	  'LIBS'
$!'f$verify(0)
$goto cloop
$!
$!
$cdone: close cfile$
$!
$!	Done
$!
$! 'f$verify(v)
$exit
$!
$!  Various DCL subroutines follow...
$!
$!  This routine takes parameter p1 to be a linker options file with a list
$!  of object files that are needed.  It extracts the names, and compiles
$!  each source module, one by one.  File names that begin with an
$!  "INSN-" are assumed to be generated by a GEN*.C program.
$!
$!  Parameter P2 is a list of files which will appear in the options file
$!  that should not be compiled.  This allows us to handle special cases.
$!
$compile:
$subroutine
$on error then goto c_err
$on control_y then goto c_err
$open ifile$ 'p1'
$loop: read ifile$ line/end=c_done
$!
$i=0
$loop1:
$flnm=f$element(i,",",line)
$i=i+1
$if flnm.eqs."" then goto loop
$if flnm.eqs."," then goto loop
$if f$locate(flnm,p2).lt.f$length(p2) then goto loop1
$! check for front-end subdirectory: "[.prfx]flnm"
$prfx = ""
$k = f$locate("]",flnm)
$if k.eq.1 then  goto loop1	![]c-common for [.cp]
$if k.lt.f$length(flnm) then  prfx = f$extract(2,k-2,flnm)
$if k.lt.f$length(flnm) then  flnm = f$extract(k+1,99,flnm)
$ if prfx.nes.""
$ then	set default [.'prfx']	!push
$	save_cflags = CFLAGS
$	CFLAGS = CFLAGS - CINCL1 - CINCL2 + CINCL_SUB
$ endif
$!
$if f$locate("parse",flnm).nes.f$length(flnm)
$	then
$	if (f$search("''flnm'.C") .eqs. "") then goto yes_bison
$	if (f$cvtime(f$file_attributes("''flnm'.Y","RDT")).les. -
 	    f$cvtime(f$file_attributes("''flnm'.C","RDT")))  -
		then goto no_bison
$yes_bison:
$set verify
$	 'BISON''BISON_FLAGS' 'flnm'.y
$	 'RENAME' 'flnm'_tab.c 'flnm'.c
$	 'RENAME' 'flnm'_tab.h 'flnm'.h
$!'f$verify(0)
$	if flnm.eqs."cp-parse" .or. (prfx.eqs."cp" .and. flnm.eqs."parse")
$	then		! fgrep '#define YYEMPTY' cp-parse.c >>cp-parse.h
$		open/Append jfile$ 'flnm'.h
$		'SEARCH'/Exact/Output=jfile$ 'flnm'.c "#define YYEMPTY"
$		close jfile$
$	endif
$no_bison:
$	 echo " (Ignore any warning about not finding file ""bison.simple"".)"
$	endif
$!
$if f$extract(0,5,flnm).eqs."insn-" then call generate 'flnm'.c
$!
$set verify
$ 'CC''CFLAGS' 'flnm'.c
$!'f$verify(0)
$ if prfx.nes.""
$ then	set default [-]		!pop
$	CFLAGS = save_CFLAGS
$ endif
$
$goto loop1
$!
$!
$! In case of error or abort, go here (In order to close file).
$!
$c_err: !'f$verify(0)
$close ifile$
$exit %x2c
$!
$c_done:
$close ifile$
$endsubroutine
$!
$! This subroutine generates the insn-* files.  The first argument is the
$! name of the insn-* file to generate.  The second argument contains a 
$! list of any other object modules which must be linked to the gen*.c
$! program.
$!
$generate:
$subroutine
$if f$extract(0,5,p1).nes."INSN-"
$	then
$	write sys$error "Unknown file passed to generate."
$	exit 1
$	endif
$root1=f$parse(f$extract(5,255,p1),,,"NAME")
$	set verify
$ 'CC''CFLAGS' GEN'root1'.C
$ 'LINK''LDFLAGS' GEN'root1'.OBJ,rtl.obj,obstack.obj,'p2' -
	  'LIBS'
$!	'f$verify(0)
$!
$set verify
$	assign/user 'p1' sys$output:
$	mcr sys$disk:[]GEN'root1' md
$!'f$verify(0)
$endsubroutine
$!
$! This subroutine generates the bc-* files.  The first argument is the
$! name of the bc-* file to generate.  The second argument contains a 
$! list of any other object modules which must be linked to the bi*.c
$! program.
$!
$bc_generate:
$subroutine
$if f$extract(0,3,p1).nes."BC-"
$	then
$	write sys$error "Unknown file passed to generate."
$	exit 1
$	endif
$root1=f$parse(f$extract(3,255,p1),,,"NAME")
$	set verify
$ 'CC''CFLAGS' BI-'root1'.C
$ 'LINK''LDFLAGS' BI-'root1'.OBJ,'p2' -
	  'LIBS'
$!	'f$verify(0)
$!
$set verify
$	assign/user bytecode.def sys$input:
$	assign/user 'p1' sys$output:
$	mcr sys$disk:[]BI-'root1'
$!'f$verify(0)
$endsubroutine
