$!
$!	Build GCC
$!
$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$! First, build the preprocesor.
$!
$ @make-cccp
$!
$! To build the GNU C++ compiler in addition to the GNU CC compiler, comment
$! out the `@make-cc1' line, and uncomment the `@make-cc1 cc1 cc1plus' line.
$! To also build Objective-C, add "cc1obj" to the list.
$!
$! See the file make-cc1.com for a complete list of options.
$!
$ @make-cc1
$! @make-cc1 cc1 cc1plus
$!
$!
$! Now build the library routines that are required.  These will be placed in
$! libgcc2.olb.  To install, extract all of the modules from libgcc2.olb and
$! add them to gnu_cc:[000000]gcclib.olb.  You may have to delete the eprintf
$! and new modules from the gnu_cc:[000000]gcclib.olb, since libgcc2 supplies
$! these same routines with different module names.
$!
$! Set up the logical names to use the compiler that we just built.
$!
$ Procedure:='f$environment("PROCEDURE")'
$ Device:='f$parse(Procedure,,,"DEVICE","NO_CONCEAL")'
$ Directory:='f$parse(Procedure,,,"DIRECTORY","NO_CONCEAL")'
$ Path:="''Device'''Directory'"
$!
$! Check for "rooted" directory specs
$!
$ l = 'f$length(Path)'
$ tmp = 'f$locate(".][",Path)'
$ if 'tmp' .ne. 'l' then goto 10$
$ tmp = 'f$locate(".><",Path)'
$ if 'tmp' .ne. 'l' then goto 10$
$ goto 100$
$!
$! Eliminate rooted directory specs
$!
$ 10$:
$ if "''f$extract(tmp,255,Path)'" .eqs. ".][000000]" then goto 20$
$ if "''f$extract(tmp,255,Path)'" .eqs. ".><000000>" then goto 20$
$ l = tmp + 3
$ Path:="''f$extract(0,tmp,Path)'.''f$extract(l,255,Path)'"
$ goto 100$
$ 20$:
$ l = tmp + 1
$ Path:="''f$extract(0,tmp,Path)'''f$extract(l,1,Path)'"
$ 100$:
$!
$! Calculate the prefix and suffix (used in generating desired paths)
$!
$ l = 'f$length(Path)' - 1
$ Prefix:='f$Extract(0,l,Path)'
$ Suffix:='f$extract(l,1,Path)'
$!
$ gnu_cc_path:="''Prefix'.''Suffix'"
$!
$oldgcc=f$trnlnm("GNU_CC")
$ assign  'gnu_cc_path'/translation=concealed, -
	'f$trnlnm("GNU_CC")/translation=concealed GNU_CC
$!
$! Set the version number from version.opt.
$!
$open ifile$ version.opt
$read ifile$ line
$close ifile$
$ijk = f$locate("=",line) + 1
$line='f$extract(ijk,255,line)
$assign 'line' gnu_cc_version
$!
$! Now build gcclib2.olb
$!
$@make-l2
$!
$! Deassign logical names.
$!
$deassign gnu_cc_version
$deassign gnu_cc
