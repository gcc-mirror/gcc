#!/bin/sh
# Run ACATS with the GNU Ada compiler

# The following functions are to be customized if you run in cross
# environment or want to change compilation flags.  Note that for
# tests requiring checks not turned on by default, this script
# automatically adds the needed flags to pass (ie: -gnato or -gnatE).

# gccflags="-O3 -fomit-frame-pointer -funroll-all-loops -finline-functions"
# gnatflags="-gnatN"

gccflags=""
gnatflags="-q -gnatws"

target_run () {
$*
}

# End of customization section.

display_noeol () {
  printf "$@"
  printf "$@" >> $dir/acats.sum
  printf "$@" >> $dir/acats.log
}

display () {
  echo "$@"
  echo "$@" >> $dir/acats.sum
  echo "$@" >> $dir/acats.log
}

dir=`pwd`

if [ "$testdir" = "" ]; then
   echo You must use make check or make check-ada
   exit 1
fi

if [ "$dir" = "$testdir" ]; then
  echo "error: srcdir must be different than objdir, exiting."
  exit 1
fi

target_gnatmake () {
  gnatmake $gnatflags $gccflags $* -largs $EXTERNAL_OBJECTS
}

target_gcc () {
  gcc $gccflags $*
}

clean_dir () {
  rm -f "$binmain" *.o *.ali > /dev/null 2>&1
}

EXTERNAL_OBJECTS=""
# Global variable to communicate external objects to link with.

rm -f $dir/acats.sum $dir/acats.log

display "	=== CONFIGURATION ==="

display `type gcc`
display `gcc -v 2>&1`
display host=`host_gcc -dumpmachine`
display target=`gcc -dumpmachine`
display `type gnatmake`
gnatls -v >> $dir/acats.log
display ""

display "	=== SUPPORT ==="
display_noeol "Generating support files..."

rm -rf $dir/support
mkdir -p $dir/support
cd $dir/support

cp $testdir/support/{*.ada,*.a,*.tst} $dir/support

sed -e "s,ACATS4GNATDIR,$dir,g" \
  < $testdir/support/impdef.a > $dir/support/impdef.a
sed -e "s,ACATS4GNATDIR,$dir,g" \
  < $testdir/support/macro.dfs > $dir/support/MACRO.DFS
sed -e "s,ACATS4GNATDIR,$dir,g" \
  < $testdir/support/tsttests.dat > $dir/support/TSTTESTS.DAT

cp $testdir/tests/cd/*.c $dir/support
cp $testdir/tests/cxb/*.c $dir/support

rm -rf $dir/run
mv $dir/tests $dir/tests.$$
rm -rf $dir/tests.$$ &
mkdir -p $dir/run

cp -pr $testdir/tests $dir/

for i in $dir/support/{*.ada,*.a}; do 
   gnatchop $i > /dev/null 2>&1
done

# These tools are used to preprocess some ACATS sources
# they need to be compiled native on the host.

host_gnatmake -q -gnatws macrosub
if [ $? -ne 0 ]; then
   display "**** Failed to compile macrosub"
   exit 1
fi
./macrosub > macrosub.out 2>&1

host_gcc -c cd300051.c
host_gnatmake -q -gnatws widechr
if [ $? -ne 0 ]; then
   display "**** Failed to compile widechr"
   exit 1
fi
./widechr > widechr.out 2>&1

rm -f $dir/support/{macrosub,widechr,*.ali,*.o}

display " done."

# From here, all compilations will be made by the target compiler

display_noeol "Compiling support files..."

target_gcc -c *.c
if [ $? -ne 0 ]; then
   display "**** Failed to compile C code"
   exit 1
fi

gnatchop *.adt > gnatchop.out 2>&1

target_gnatmake -c -gnato -gnatE *.ads > /dev/null 2>&1
target_gnatmake -c -gnato -gnatE *.adb

display " done."
display ""
display "	=== ACATS tests ==="

if [ $# -eq 0 ]; then
   chapters=`cd $dir/tests; echo [a-z]*`
else
   chapters=$*
fi

glob_countn=0
glob_countok=0

for chapter in $chapters; do
   display Running chapter $chapter ...

   if [ ! -d $dir/tests/$chapter ]; then
      display "*** CHAPTER $chapter does not exist, skipping."
      display ""
      continue
   fi

   cd $dir/tests/$chapter
   ls *.{a,ada,adt,am,dep} 2> /dev/null | sed -e 's/\(.*\)\..*/\1/g' | \
   cut -c1-7 | sort | uniq | comm -23 - $testdir/norun.lst \
     > $dir/tests/$chapter/${chapter}.lst 
   countn=`wc -l < $dir/tests/$chapter/${chapter}.lst`
   countok=0
   counti=0
   for i in `cat $dir/tests/$chapter/${chapter}.lst`; do 
      counti=`expr $counti + 1`
      extraflags=""
      grep $i $testdir/overflow.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -gnato"
      fi
      grep $i $testdir/elabd.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -gnatE"
      fi
      mkdir $dir/tests/$chapter/$i
      cd $dir/tests/$chapter/$i
      gnatchop -c -w `ls $dir/tests/${chapter}/${i}*.{a,ada,adt,am,dep} 2> /dev/null` > /dev/null 2>&1
      ls ${i}?.adb > ${i}.lst 2> /dev/null
      ls ${i}*m.adb >> ${i}.lst 2> /dev/null
      ls ${i}.adb >> ${i}.lst 2> /dev/null
      main=`tail -1 ${i}.lst`
      binmain=`echo $main | sed -e 's/\(.*\)\..*/\1/g'`
      echo "BUILD $main" >> $dir/acats.log
      EXTERNAL_OBJECTS=""
      case $i in
        cxb30*) EXTERNAL_OBJECTS="$dir/support/cxb30040.o $dir/support/cxb30060.o $dir/support/cxb30130.o $dir/support/cxb30131.o";;
        ca1020e) rm -f ca1020e_func1.adb ca1020e_func2.adb ca1020e_proc1.adb ca1020e_proc2.adb > /dev/null 2>&1;;
        ca14028) rm -f ca14028_func2.ads ca14028_func3.ads ca14028_proc1.ads ca14028_proc3.ads > /dev/null 2>&1;;
        cxh1001) extraflags="-a -f"; echo "pragma Normalize_Scalars;" > gnat.adc
      esac
      if [ "$main" = "" ]; then
         display "FAIL:	$i"
         failed="${failed}${i} "
         clean_dir
         continue
      fi

      target_gnatmake $extraflags -I$dir/support $main >> $dir/acats.log 2>&1
      if [ $? -ne 0 ]; then
         display "FAIL:	$i"
         failed="${failed}${i} "
         clean_dir
         continue
      fi

      echo "RUN $binmain" >> $dir/acats.log
      cd $dir/run
      target_run $dir/tests/$chapter/$i/$binmain > $dir/tests/$chapter/$i/${i}.log 2>&1
      cd $dir/tests/$chapter/$i
      cat ${i}.log >> $dir/acats.log
      egrep -e '(==== |\+\+\+\+ |\!\!\!\! )' ${i}.log > /dev/null 2>&1
      if [ $? -ne 0 ]; then
         display "FAIL:	$i"
         failed="${failed}${i} "
      else
         display "PASS:	$i"
         countok=`expr $countok + 1`
      fi
      clean_dir
   done

   glob_countok=`expr $glob_countok + $countok`
   glob_countn=`expr $glob_countn + $countn`
done

display "	=== ACATS Summary ==="
display "# of expected passes $glob_countok"
display "# of unexpected failures `expr $glob_countn - $glob_countok`"

if [ $glob_countok -ne $glob_countn ]; then
   display "*** FAILURES: $failed"
fi

exit 0
