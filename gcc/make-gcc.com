$!
$!	Build GCC
$!
$! Set the def dir to proper place for use in batch. Works for interactive too.
$flnm = f$enviroment("PROCEDURE")     ! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$set symbol/scope=(nolocal,noglobal)
$!
$! First, build the preprocessor.
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
$! Now build gcclib2.olb
$!
$ @make-l2
$!
