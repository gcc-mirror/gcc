$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$ arch_indx = 1 + ((f$getsyi("CPU").ge.128).and.1)      ! vax==1, alpha==2
$ arch = f$element(arch_indx,"|","|vax|alpha|")
$!
$! Command file to build libgcc2.olb.  You should only run this once you 
$! have the current compiler installed, otherwise some of the builtins will
$! not be recognized.  Once you have built libgcc2.olb, you can merge this
$! with gnu:[000000]gcclib.olb
$!
$! All of the source code is assumed to be in libgcc2.c, and a list of the
$! modules that we need from there is in libgcc2.list (which is generated
$! when config-gcc.com is run).
$!
$if f$search("gcc-cc1.exe").eqs.""
$  then
$    gcc-cc1:=$gnu_cc_library:gcc-cc1
$  else
$    gcc_cc1:=$sys$disk:[]gcc-cc1
$  endif
$!
$if f$search("gcc-cpp.exe").eqs.""
$  then
$    gcc_cpp:=$gnu_cc_library:gcc-cpp
$  else
$    gcc_cpp:=$sys$disk:[]gcc-cpp
$    open ifile$ version.opt
$    read ifile$ line
$    close ifile$
$  endif
$!
$ gcc_as:=$gnu_root:[bin]as       
$ cpp_file:=sys$scratch:gcc_'f$getjpi(0,"pid")'.cpp
$ s_file:=sys$scratch:gcc_'f$getjpi(0,"pid")'.s
$!
$set symbol/scope=(nolocal,noglobal)
$!
$lib/create libgcc2.olb
$on error then goto c_err
$on control_y then goto c_err
$open ifile$ libgcc2.list
$loop: read ifile$ line/end=c_done
$!
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
$goto loop
$!
$! In case of error or abort, go here (In order to close file).
$!
$c_err: !'f$verify(0)
$close ifile$
$ exit %x2c
$!
$c_done:
$close ifile$
