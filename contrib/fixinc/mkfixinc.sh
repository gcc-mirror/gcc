#! /bin/sh

machine=$1
dest=$2

if test -z "$dest"
then
     echo "No destination directory specified"
     exit 1
fi

echo constructing $dest for $machine

case $machine in
    alpha*-dec-vms* | \
    arm-semi-aout   | \
    armel-semi-aout | \
    arm-semi-aof    | \
    armel-semi-aof | \
    c1-convex-*  | \
    c2-convex-*  | \
    c32-convex-* | \
    c34-convex-* | \
    c38-convex-* | \
    hppa1.[01]-*-osf* | \
    hppa1.[01]-*-bsd* | \
    hppa*-*-lites* | \
    i[34567]86-moss-msdos* | \
    i[34567]86-*-moss* | \
    i[34567]86-*-sysv5* | \
    i[34567]86-*-osf1* | \
    i[34567]86-*-pe | \
    i[34567]86-*-cygwin32 | \
    i[34567]86-*-mingw32* | \
    mips-sgi-irix5cross64 | \
    mips-dec-bsd* | \
    powerpc-*-eabi* | \
    powerpc-*-rtems* | \
    powerpcle-*-eabi* | \
    powerpcle-*-winnt*  | \
    powerpcle-*-pe | \
    powerpcle-*-cygwin32  | \
    *-*-linux-* | \
    *-*-gnu*    )
        fixincludes=`basename $dest`
        ;;

    i[34567]86-dg-dgux* | \
    m88k-dg-dgux*)
        fixincludes=fixinc.dgux
        ;;

    mips-sgi-irix[56]* )
        fixincludes=fixinc.irix
        ;;

    i[34567]86-sequent-ptx1* | \
    i[34567]86-sequent-ptx2* | \
    i[34567]86-sequent-ptx4* | \
    i[34567]86-sequent-sysv3* | \
    i[34567]86-sequent-sysv4*)
        fixincludes=fixinc.ptx
        ;;

    i[34567]86-*-sco3.2v[45]*)
        fixincludes=fixinc.sco
        ;;

    i[34567]86-*-solaris2.[0-4]* | \
    *-*-solaris2.[0-4]* | \
    *-*-sysv4*)
        fixincludes=fixinc.svr4
        ;;

    alpha*-*-winnt* | \
    i[34567]86-*-winnt3*)
        fixincludes=fixinc.winnt
        ;;

    alpha*-*-netbsd* | \
    arm-*-netbsd* | \
    i[34567]86-*-freebsdelf* | \
    i[34567]86-*-freebsd* | \
    i[34567]86-*-netbsd* | \
    i[34567]86-*-solaris2* | \
    m68k-*-netbsd* | \
    mips-dec-netbsd* | \
    ns32k-pc532-netbsd* | \
    *-*-solaris2* | \
    sparc-*-netbsd* | \
    vax-*-netbsd*)
        fixincludes=fixinc.wrap
        ;;

    *)
        fixincludes=`basename $dest`
        ;;
esac

if test -z "$fixincludes"
then
    cat > $dest  <<-	_EOF_
	#! /bin/sh
	exit 0
	_EOF_
    exit 0
fi

if test -f "$fixincludes"
then
    echo copying $fixincludes to $dest
    cp $fixincludes $dest
    exit 0
fi

echo $MAKE install DESTDIR=`dirname $dest`
$MAKE install DESTDIR=`dirname $dest`

exit 1
