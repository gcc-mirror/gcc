$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$!
$! CAUTION: If you want to link gcc-cc1 to the sharable image library
$! VAXCRTL, see the notes in gcc.texinfo (or INSTALL) first.
$!
$!	Build a GNU compiler on VMS
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
$p1 = p1+" "+p2+" "+p3+" "+p4+" "+p5+" "+p6+" "+p7 
$p1 = f$edit(p1,"COMPRESS")
$i=0
$DO_ALL = 0
$DO_LINK = 0
$DO_DEBUG = 0
$DO_CC1 = 0
$DO_CC1PLUS = 0
$DO_CC1OBJ = 0
$DO_INDEPENDENT = 1
$DO_DEFAULT = 1
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
$if DO_ALL.eq.1 then DO_CC1 = 1
$if DO_ALL.eq.1 then DO_CC1PLUS = 1
$if DO_ALL.eq.1 then DO_CC1OBJ = 1
$say:==write sys$Output
$say "This command file will now perform the following actions:
$if DO_LINK.eq.1 then goto link_only
$if DO_CC1.eq.1 then say "   Compile C specific object modules."
$if DO_CC1PLUS.eq.1 then say "   Compile C++ specific object modules."
$if DO_CC1OBJ.eq.1 then say "   Compile obj-C specific object modules."
$if DO_INDEPENDENT.eq.1 then say "   Compile language independent object modules."
$link_only:
$if DO_CC1.eq.1 then 	say "   Link C compiler (gcc-cc1.exe)."
$if DO_CC1PLUS.eq.1 then say "   Link C++ compiler (gcc-cc1plus.exe)."
$if DO_CC1OBJ.eq.1 then say "   Link objective-C compiler (gcc-cc1obj.exe)."
$if DO_DEBUG.eq.1 then say  "   Link images to run under debugger."
$type sys$input

	Note: GCC 2.0 treats external variables differently than GCC 1.40 does.
Before you use GCC 2.0, you should obtain a version of the assembler which 
contains the patches to work with GCC 2.0 (GCC-AS 1.38 does not contain 
these patches - whatever comes after this probably will).

	If you do not update the assembler, the compiler will still work,
but `extern const' variables will be treated as `extern'.  This will result
in linker warning messages about mismatched psect attributes, and these
variables will be placed in read/write storage.

$!
$!
$!
$! CAUTION: If you want to link gcc-cc1 to the sharable image library
$! VAXCRTL, see the notes in gcc.texinfo (or INSTALL) first.
$!
$!	Build the GNU "C" compiler on VMS
$!   (To try to build with VAX C, replace `gcc' with `cc/noopt'
$!    and delete `cc1_options="-mpcc-alignment"'.
$!    Also add `/sel' after `gcclib/lib' except in the last link.
$!    You also need to get alloca.mar from Bison
$!    and to make definitions for bzero, bcopy and bcmp.)
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
$ CFLAGS =	"/debug/cc1_options=""-mpcc-alignment""/inc=([],[.config])"
$!
$!	Link options
$!
$ LDFLAGS :=	/nomap
$ if DO_DEBUG.eq.1 then LDFLAGS :='LDFLAGS'/debug
$!
$!	Link libraries
$!
$ LIBS :=	gnu_cc:[000000]gcclib/libr,sys$share:vaxcrtl/libr
$!
$!
$!
$!
$! Language independent object and header files.
$!
$! create a linker options file that lists all of the language independent
$! object modules.
$!
$create independent.opt
!
! List of object files for the linker - these are language independent
! (i.e. the same files will be used for all of the compilers).
!
toplev,tree,print-tree,stor-layout,fold-const,varasm,rtl,rtlanal,expr,stmt
expmed,explow,optabs,emit-rtl,insn-emit,jump,cse,loop,flow,stupid,combine
regclass,local-alloc,global-alloc,reload,reload1,insn-peep,final,recog
insn-recog,insn-extract,insn-output,obstack,integrate,caller-save,calls
dwarfout,xcoffout,function,insn-attrtab,reorg,sched,sdbout,dbxout,unroll
reg-stack,aux-output,print-rtl,getpwd,version
$!
$pur/nolog independent.opt
$!
$ if DO_LINK.eq.1 then goto compile_cc1
$if DO_INDEPENDENT.eq.0 THEN GOTO compile_cc1
$!
$! First build a couple of header files from the machine description
$! These are used by many of the source modules, so we build them now.
$!
$	'CC 'CFLAGS rtl.c
$	'CC 'CFLAGS obstack.c
$	'CC 'CFLAGS print-rtl.c
$! Generate insn-attr.h
$	'CC 'CFLAGS genattr.c
$	link 'LDFLAGS' genattr,rtl,obstack, 'LIBS'
$	assign/user insn-attr.h sys$output:
$	mcr sys$disk:[]genattr md
$! Generate insn-flags.h
$	'CC 'CFLAGS genflags.c
$	link 'LDFLAGS' genflags,rtl,obstack, 'LIBS'
$	assign/user insn-flags.h sys$output:
$	mcr sys$disk:[]genflags md
$! Generate insn-codes.h
$	'CC 'CFLAGS gencodes.c
$	link 'LDFLAGS' gencodes,rtl,obstack, 'LIBS'
$	assign/user insn-codes.h sys$output:
$	mcr sys$disk:[]gencodes md
$! Generate insn-config.h
$	'CC 'CFLAGS genconfig.c
$	link 'LDFLAGS' genconfig,rtl,obstack, 'LIBS'
$	assign/user insn-config.h sys$output:
$	mcr sys$disk:[]genconfig md
$!
$! Now compile the source modules
$!
$	'CC 'CFLAGS toplev.c
$	'CC 'CFLAGS version.c
$	'CC 'CFLAGS tree.c
$	'CC 'CFLAGS print-tree.c
$	'CC 'CFLAGS stor-layout.c
$	'CC 'CFLAGS fold-const.c
$	'CC 'CFLAGS varasm.c
$	'CC 'CFLAGS expr.c
$	'CC 'CFLAGS stmt.c
$	'CC 'CFLAGS expmed.c
$	'CC 'CFLAGS explow.c
$	'CC 'CFLAGS optabs.c
$	'CC 'CFLAGS rtlanal.c
$	'CC 'CFLAGS emit-rtl.c
$! Generate insn-emit.c
$	'CC 'CFLAGS genemit.c
$	link 'LDFLAGS' genemit,rtl,obstack, 'LIBS'
$	assign/user insn-emit.c sys$output:
$	mcr sys$disk:[]genemit md
$!
$	'CC 'CFLAGS insn-emit.c
$	'CC 'CFLAGS jump.c
$	'CC 'CFLAGS cse.c
$	'CC 'CFLAGS loop.c
$	'CC 'CFLAGS flow.c
$	'CC 'CFLAGS stupid.c
$	'CC 'CFLAGS combine.c
$	'CC 'CFLAGS regclass.c
$	'CC 'CFLAGS local-alloc.c
$	'CC 'CFLAGS global-alloc.c
$	'CC 'CFLAGS reload.c
$	'CC 'CFLAGS reload1.c
$! Generate insn-peep.c
$	'CC 'CFLAGS genpeep.c
$	link 'LDFLAGS' genpeep,rtl,obstack, 'LIBS'
$	assign/user insn-peep.c sys$output:
$	mcr sys$disk:[]genpeep md
$!
$	'CC 'CFLAGS insn-peep.c
$	'CC 'CFLAGS final.c
$	'CC 'CFLAGS recog.c
$! Generate insn-recog.c
$	'CC 'CFLAGS genrecog.c
$	link 'LDFLAGS' genrecog,rtl,obstack, 'LIBS'
$	assign/user insn-recog.c sys$output:
$	mcr sys$disk:[]genrecog md
$!
$	'CC 'CFLAGS insn-recog.c
$! Generate insn-extract.c
$	'CC 'CFLAGS genextract.c
$	link 'LDFLAGS' genextract,rtl,obstack, 'LIBS'
$	assign/user insn-extract.c sys$output:
$	mcr sys$disk:[]genextract md
$!
$	'CC 'CFLAGS insn-extract.c
$! Generate insn-output.c
$	'CC 'CFLAGS genoutput.c
$ 	link 'LDFLAGS' genoutput,rtl,obstack, 'LIBS'
$	assign/user insn-output.c sys$output:
$	mcr sys$disk:[]genoutput md
$!
$	'CC 'CFLAGS insn-output.c
$	'CC 'CFLAGS integrate.c
$	'CC 'CFLAGS caller-save.c
$	'CC 'CFLAGS calls.c
$	'CC 'CFLAGS dwarfout.c
$	'CC 'CFLAGS dbxout.c
$	'CC 'CFLAGS xcoffout.c
$	'CC 'CFLAGS reg-stack.c
$	'CC 'CFLAGS function.c
$	'CC 'CFLAGS reorg.c
$	'CC 'CFLAGS sched.c
$	'CC 'CFLAGS sdbout.c
$	'CC 'CFLAGS unroll.c
$! Generate insn-attrtab.c
$	'CC 'CFLAGS genattrtab.c
$	link 'LDFLAGS' genattrtab,rtl,rtlanal,obstack, 'LIBS'
$	assign/user insn-attrtab.c sys$output:
$	mcr sys$disk:[]genattrtab md
$	'CC 'CFLAGS insn-attrtab.c
$	'CC 'CFLAGS aux-output.c
$	'CC 'CFLAGS getpwd.c
$!
$compile_cc1:
$!
$! C language specific modules
$!
$if DO_CC1.eq.0 then goto compile_cc1plus
$if DO_LINK.eq.1 then goto link_cc1
$!
$	if (f$search("C-PARSE.C") .eqs. "") then goto gcc_bison
$	if (f$cvtime(f$file_attributes("C-PARSE.Y","RDT")).les. -
 	    f$cvtime(f$file_attributes("C-PARSE.C","RDT")))  -
		then goto gcc_nobison
$gcc_bison:  'BISON' /define /verbose c-parse.y
$	 'RENAME' c-parse_tab.c c-parse.c
$	 'RENAME' c-parse_tab.h c-parse.h
$gcc_nobison:
$	'CC 'CFLAGS c-parse.c
$	'CC 'CFLAGS c-lex.c
$	'CC 'CFLAGS c-decl.c
$	'CC 'CFLAGS c-typeck.c
$	'CC 'CFLAGS c-convert.c
$	'CC 'CFLAGS c-aux-info.c
$	'CC 'CFLAGS c-common.c
$	'CC 'CFLAGS c-lang.c
$!
$! CAUTION: If you want to link gcc-cc1 to the sharable image library
$! VAXCRTL, see the notes in gcc.texinfo (or INSTALL) first.
$!
$link_cc1:
$ link 'LDFLAGS' /exe=gcc-cc1  version.opt/opt,sys$input:/opt, -
	independent.opt/opt,'LIBS'
!
!	"CC1" Linker options file
!
!
c-parse,c-decl,c-typeck,c-convert,c-aux-info,c-common,c-lang,c-lex
$!
$! C++ language specific modules
$!
$compile_cc1plus:
$!
$if DO_CC1PLUS.eq.0 then goto compile_cc1obj
$if DO_LINK.eq.1 then goto link_cc1plus
$!
$	if (f$search("CP-PARSE.C") .eqs. "") then goto cplus_bison
$	if (f$cvtime(f$file_attributes("CP-PARSE.Y","RDT")).les. -
 	    f$cvtime(f$file_attributes("CP-PARSE.C","RDT")))  -
  		then goto cplus_nobison
$cplus_bison:
$	 'BISON' /define /verbose cp-parse.y
$	 'RENAME' cp-parse_tab.c cp-parse.c
$	 'RENAME' cp-parse_tab.h cp-parse.h
$cplus_nobison:
$!
$	'CC 'CFLAGS cp-parse.c
$	'CC 'CFLAGS cp-decl.c
$	'CC 'CFLAGS cp-decl2.c
$	'CC 'CFLAGS cp-typeck.c
$	'CC 'CFLAGS cp-type2.c
$	'CC 'CFLAGS cp-tree.c
$	'CC 'CFLAGS cp-ptree.c
$	'CC 'CFLAGS cp-cvt.c
$	'CC 'CFLAGS cp-search.c
$	'CC 'CFLAGS cp-lex.c
$	'CC 'CFLAGS cp-gc.c
$	'CC 'CFLAGS cp-call.c
$	'CC 'CFLAGS cp-class.c
$	'CC 'CFLAGS cp-init.c
$	'CC 'CFLAGS cp-method.c
$	'CC 'CFLAGS cp-except.c
$	'CC 'CFLAGS cp-expr.c
$	'CC 'CFLAGS cp-pt.c
$	'CC 'CFLAGS cp-edsel.c
$	'CC 'CFLAGS cp-xref.c
$	'CC 'CFLAGS cp-spew.c
$	'CC 'CFLAGS c-common.c
$!
$link_cc1plus:
$ link 'LDFLAGS' /exe=gcc-cc1plus  version.opt/opt,sys$input:/opt, -
		independent.opt/opt,'LIBS'
!
!	"CC1PLUS" Linker options file
!
cp-parse,cp-decl,cp-decl2,cp-typeck,cp-type2,cp-tree
cp-ptree,cp-cvt,cp-search,cp-lex,cp-gc,cp-call,cp-class
cp-init,cp-method,cp-except,cp-expr,cp-pt,cp-edsel
cp-xref,cp-spew,c-common
$!
$! objective language specific modules
$!
$compile_cc1obj:
$if DO_CC1OBJ.eq.0 then goto all_done
$if DO_LINK.eq.1 then goto LINK_CC1OBJ
$!
$	if (f$search("OBJC-PARSE.C") .eqs. "") then goto objc_bison
$	if (f$cvtime(f$file_attributes("OBJC-PARSE.Y","RDT")).les. -
 	    f$cvtime(f$file_attributes("OBJC-PARSE.C","RDT")))  -
		then goto objc_nobison
$objc_bison:
$	 'BISON' /define /verbose OBJC-PARSE.y
$	 'RENAME' OBJC-PARSE_tab.c OBJC-PARSE.c
$	 'RENAME' OBJC-PARSE_tab.h OBJC-PARSE.h
$objc_nobison:
$	'CC 'CFLAGS objc-parse.c
$	'CC 'CFLAGS objc-actions.c
$!
$! If have also built CC1, we do not need to recompile these modules.
$!
$if DO_CC1.eq.1 then goto LINK_CC1OBJ
$	'CC 'CFLAGS c-lex.c
$	'CC 'CFLAGS c-decl.c
$	'CC 'CFLAGS c-typeck.c
$	'CC 'CFLAGS c-convert.c
$	'CC 'CFLAGS c-aux-info.c
$	'CC 'CFLAGS c-common.c
$!
$!
$LINK_CC1OBJ:
$ link 'LDFLAGS' /exe=gcc-cc1obj version.opt/opt,sys$input:/opt, -
	independent.opt/opt,'LIBS'
!
!	"Objective C" Linker options file
!
objc-parse,objc-actions,c-lex,c-decl,c-typeck,c-convert,c-aux-info,c-common
$!
$all_done:
$!
$!	Done
$!
