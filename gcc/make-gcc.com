$! make-gcc.com -- VMS build procedure for GNU CC.
$!
$!	Usage:
$!	  $ @make-gcc.com [host-compiler] [component list]
$!
$!	where [host-compiler] is one of "GNUC", "VAXC", "DECC";
$!	default when none specified is "GNUC",
$!	and where [component list] is space separated list beginning
$!	with "CC1" and optionally followed by "CC1PLUS"; default if
$!	nothing is specified is "CC1" (the C compiler); choosing
$!	"CC1PLUS" (the C++ compiler) without also specifying "CC1"
$!	will not work.	(See make-cc1.com for other potential component
$!	values; but unless you're developing or debugging the compiler
$!	suite itself, the two above are the only ones of interest.)
$!
$!	  For a "stage 2" or subsequent build, always specify GNUC as
$!	the host compiler.
$!
$!	Note:
$!	  Even though it is possible to build with VAX C or DEC C,
$!	a prior version of the gcc-vms binary distribution is still
$!	required to be able to use the newly built GNU CC compiler(s),
$!	because the gcc source distribution does not supply the driver
$!	program which the DCL command "GCC" implements or the C header
$!	files and gcclib support library.
$!
$
$!
$! Change working directory to the location of this procedure.
$!
$ flnm = f$enviroment("PROCEDURE")	!get current procedure name
$ set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$
$!
$! First, we build the preprocessor.
$!
$ @make-cccp.com 'p1' 'p2'
$!
$! To install it, copy the resulting GCC-CPP.EXE to the GNU_CC:[000000]
$! directory.
$!
$
$!
$! Now we build the C compiler.  To build the C++ compiler too, use
$! $ @make-gcc GNUC cc1 cc1plus
$! when invoking this command procedure.  Note that you should not
$! do this for a "stage 1" build.
$!
$ @make-cc1.com 'p1' 'p2' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8'
$!
$! To install it (them), copy the resulting GCC-CC1.EXE (and GCC-CC1PLUS.EXE)
$! to the GNU_CC:[000000] directory.
$!
$
$!
$! Now we build the `libgcc2' support library.	It will need to be merged
$! with the existing gcclib.olb library.
$!
$ @make-l2.com 'p1' 'p2' 'p3' 'p4' 'p5' 'p6' 'p7' 'p8'
$!
$! To install, save a backup copy of GNU_CC:[000000]GCCLIB.OLB somewhere,
$! then update the original using the newly created LIBGCC2.OLB via
$! $ library/Obj libgcc2.olb /Extract=*/Output=libgcc2.obj
$! $ library/Obj gnu_cc:[000000]gcclib.olb libgcc2.obj /Replace
$!
$! Depending upon how old your present gcclib library is, you might have
$! to delete some modules, such as `eprintf' and `new', to avoid conflicting
$! symbols from obsolete routines.  After deleting any such modules, just
$! repeat the `library/replace' step.
$!
$ exit
