$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$@$diska:[eric]test_gcc2
$!
$! Command file to build libgcc2.olb.  You should only run this once you 
$! have the current compiler installed, otherwise some of the builtins will
$! not be recognized.  Once you have built libgcc2.olb, you can merge this
$! with gnu_cc:[000000]gcclib.olb
$!
$lib/create libgcc2.olb
$call compile_libgcc2 "L_muldi3"
$call compile_libgcc2 "L_divdi3"
$call compile_libgcc2 "L_moddi3"
$call compile_libgcc2 "L_udivdi3"
$call compile_libgcc2 "L_umoddi3"
$call compile_libgcc2 "L_negdi2"
$call compile_libgcc2 "L_lshrdi3"
$call compile_libgcc2 "L_lshldi3"
$call compile_libgcc2 "L_ashldi3"
$call compile_libgcc2 "L_ashrdi3"
$call compile_libgcc2 "L_udivmoddi4"
$call compile_libgcc2 "L_cmpdi2"
$call compile_libgcc2 "L_ucmpdi2"
$call compile_libgcc2 "L_floatdidf"
$call compile_libgcc2 "L_floatdisf"
$call compile_libgcc2 "L_fixunsdfsi"
$call compile_libgcc2 "L_fixunssfsi"
$call compile_libgcc2 "L_fixunsdfdi"
$call compile_libgcc2 "L_fixdfdi"
$call compile_libgcc2 "L_fixunssfdi"
$call compile_libgcc2 "L_fixsfdi"
$call compile_libgcc2 "L_varargs"
$call compile_libgcc2 "L_eprintf"
$call compile_libgcc2 "L_builtin_new"
$call compile_libgcc2 "L_builtin_New" L_builtin_nnew
$call compile_libgcc2 "L_builtin_del"
$call compile_libgcc2 "L_bb"
$call compile_libgcc2 "L_shtab"
$call compile_libgcc2 "L_clear_cache"
$call compile_libgcc2 "L_trampoline"
$call compile_libgcc2 "L__main"
$!call compile_libgcc2 "L_exit"
$exit
$!
$compile_libgcc2:
$subroutine
$objname = p1
$if p2.nes."" then objname = p2
$gcc/machine/include=([],[.config])/debug/define="''p1'" libgcc2.c/obj='objname'.obj
$lib libgcc2.olb 'objname'.obj
$del 'objname'.obj;/nolog
$endsubroutine
