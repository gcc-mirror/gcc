$! make-l2.com -- VMS build procedure for libgcc2.
$
$! Change working directory to the location of this command procedure.
$ flnm = f$enviroment("PROCEDURE")	!get current procedure name
$ set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$! Command file to build libgcc2.olb.  You should only run this once you
$! have the current compiler installed, otherwise some of the builtins will
$! not be recognized.  Once you have built libgcc2.olb, you can merge this
$! with gnu_cc:[000000]gcclib.olb
$!
$! All of the C source code is assumed to be in libgcc2.c, and a list of the
$! modules that we need from there is in libgcc2.list (which is generated
$! when vmsconfig.com is run).  The C++ source is found in the [.cp.inc]
$! directory and managed via libgcc2-cxx.list.
$!
$ if f$search("gcc-cc1.exe").eqs.""
$ then
$    gcc_cc1:=$gnu_cc:[000000]gcc-cc1
$    if f$extract(0,1,f$trnlnm("GNU_CC_VERSION")).eqs."1" then goto nocompile
$ else
$    gcc_cc1:=$sys$disk:[]gcc-cc1
$ endif
$!
$ if f$search("gcc-cpp.exe").eqs.""
$ then
$    gcc_cpp:=$gnu_cc:[000000]gcc-cpp
$    if f$extract(0,1,f$trnlnm("GNU_CC_VERSION")).eqs."1" then goto nocompile
$    Version:='f$trnlnm("GNU_CC_VERSION")'
$ else
$    gcc_cpp:=$sys$disk:[]gcc-cpp
$    open ifile$ version.opt
$    read ifile$ line
$    close ifile$
$    Version=line - "ident=""" - """
$ endif
$!
$ if f$search("gcc-cc1plus.exe").eqs.""
$ then	gcc_cxx = "$gnu_cc:[000000]gcc-cc1plus"
$ else	gcc_cxx = "$sys$disk:[]gcc-cc1plus"
$ endif
$!
$gcc_as:=$gnu_cc:[000000]gcc-as
$cpp_file:=sys$scratch:gcc_'f$getjpi(0,"pid")'.cpp
$ s_file:=sys$scratch:gcc_'f$getjpi(0,"pid")'.s
$!
$set symbol/scope=(nolocal,noglobal)
$!
$lib/create libgcc2.olb
$on error then goto c_err
$on control_y then goto c_err
$
$if f$trnlnm("IFILE$").nes."" then  close/noLog ifile$
$open ifile$ libgcc2.list
$loop:
$!
$read ifile$ line/end=c_done
$i=0
$loop1:
$flnm=f$element(i," ",line)
$i=i+1
$if flnm.eqs."" then goto loop
$if flnm.eqs." " then goto loop
$!
$flnm = "L"+flnm
$if flnm.eqs."L_exit" then goto loop1
$write sys$output "$ gcc/debug/define=""''flnm'"" LIBGCC2.C"
$!
$objname = flnm
$if flnm.eqs."L_builtin_New" then objname = "L_builtin_nnew"
$!
$! We do this by hand, since the VMS compiler driver does not have a way
$! of specifying an alternate location for the compiler executables.
$!
$ if arch .eqs. "alpha"
$ then
$   gcc_cpp "-D__IEEE_FLOAT" "-I[]" "-I[.config]" "-I[.ginclude]" "-D''flnm'"  libgcc2.c 'cpp_file'
$   gcc_cc1 'cpp_file' -dumpbase 'objname' -
	-quiet -mgas "-O1" -mfloat-ieee -o 's_file'
$ else
$   gcc_cpp "-I[]" "-I[.config]" "-I[.ginclude]" "-D''flnm'"  libgcc2.c 'cpp_file'
$   gcc_cc1 'cpp_file' -dumpbase 'objname' -
	-quiet -mgnu -g "-O1" -mvaxc-alignment   -o 's_file'
$ endif
$ delete/nolog 'cpp_file';
$   gcc_as 's_file'  -o 'objname'.OBJ
$ if arch .eqs. "vax"
$ then
$! Assemble again, preserving lowercase symbol names this time.
$   gcc_as -h3 's_file'  -o 'objname'-c.OBJ
$   library libgcc2.olb 'objname'.obj,'objname'-c.obj
$   delete/nolog 'objname'.obj;,'objname'-c.obj;
$ else
$   library libgcc2.olb 'objname'.obj
$   delete/nolog 'objname'.obj;
$ endif
$ delete/nolog 's_file';
$!
$!
$goto loop1
$!
$! In case of error or abort, go here (In order to close file).
$!
$c_err:	!'f$verify(0)
$close ifile$
$ exit %x2c
$!
$c_done:
$close ifile$
$
$
$ EXIT
$	!gcc-2.8.0: C++ libgcc2 code disabled since it's not adequately tested
$
$!
$ p1 = p1+" "+p2+" "+p3+" "+p4+" "+p5+" "+p6+" "+p7+" "+p8
$ p1 = " " + f$edit(p1,"COMPRESS,TRIM,UPCASE") + " "
$! (note: substring locations can only be equal when neither string is present)
$ if f$locate(" CC1PLUS ",p1).eq.f$locate(" CXX ",p1) then  goto cxx_done
$ if f$search("libgcc2-cxx.list").eqs."" then  goto cxx_done
$!
$ open/read ifile$ libgcc2-cxx.list
$cxx_line_loop:
$ read ifile$ line/end=cxx_done
$ i = 0
$cxx_file_loop:
$ flnm = f$element(i,",",line)
$ i = i + 1
$ if flnm.eqs."" .or. flnm.eqs."," then goto cxx_line_loop
$ write sys$output "$ gcc/plus/debug ''flnm'.cc"
$ objname = flnm
$!
$ gcc_cpp -+ "-I[]" "-I[.ginclude]" "-I[.cp.inc]" [.cp]'flnm'.cc 'cpp_file'
$ gcc_cxx 'cpp_file' -dumpbase 'objname' -fexceptions -
	-quiet -mgnu -g "-O1" -mvaxc-alignment	 -o 's_file'
$ delete/nolog 'cpp_file';
$ gcc_as "-vGNU CC  V''Version'" 's_file'  -o 'objname'.OBJ
$! Assemble again, preserving lowercase symbol names this time.
$ gcc_as "-vGNU CC  V''Version'" -h3 's_file'  -o 'objname'-c.OBJ
$ delete/nolog 's_file';
$
$ library libgcc2.olb 'objname'.obj,'objname'-c.obj
$ delete/nolog 'objname'.obj;,'objname'-c.obj;
$!
$ goto cxx_file_loop
$!
$cxx_done:
$ close ifile$
$ exit
