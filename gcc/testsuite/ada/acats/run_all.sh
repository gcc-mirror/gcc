#!/bin/sh
# Run ACATS with the GNU Ada compiler

# The following functions are to be customized if you run in cross
# environment or want to change compilation flags.  Note that for
# tests requiring checks not turned on by default, this script
# automatically adds the needed flags to pass (ie: -gnato or -gnatE).

# gccflags="-O3 -fomit-frame-pointer -funroll-all-loops -finline-functions"
# gnatflags="-gnatN"

gccflags="-O2"
gnatflags="-gnatws"

target_run () {
  eval $EXPECT -f $testdir/run_test.exp $*
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

log () {
  echo "$@" >> $dir/acats.sum
  echo "$@" >> $dir/acats.log
}

dir=`${PWDCMD-pwd}`

if [ "$testdir" = "" ]; then
   echo You must use make check or make check-ada
   exit 1
fi

if [ "$dir" = "$testdir" ]; then
  echo "error: srcdir must be different than objdir, exiting."
  exit 1
fi

target_gnatchop () {
  gnatchop --GCC="$GCC_DRIVER" $*
}

target_gnatmake () {
  echo gnatmake --GCC=\"$GCC\" $gnatflags $gccflags $* -largs $EXTERNAL_OBJECTS --GCC=\"$GCC\"
  gnatmake --GCC="$GCC" $gnatflags $gccflags $* -largs $EXTERNAL_OBJECTS --GCC="$GCC"
}

target_gcc () {
  $GCC $gccflags $*
}

clean_dir () {
  rm -f "$binmain" *.o *.ali > /dev/null 2>&1
}

find_main () {
  ls ${i}?.adb > ${i}.lst 2> /dev/null
  ls ${i}*m.adb >> ${i}.lst 2> /dev/null
  ls ${i}.adb >> ${i}.lst 2> /dev/null
  main=`tail -1 ${i}.lst`
}

EXTERNAL_OBJECTS=""
# Global variable to communicate external objects to link with.

rm -f $dir/acats.sum $dir/acats.log

display "Test Run By $USER on `date`"

display "		=== acats configuration ==="

target=`$GCC -dumpmachine`

display target gcc is $GCC
display `$GCC -v 2>&1`
display host=`gcc -dumpmachine`
display target=$target
display `type gnatmake`
gnatls -v >> $dir/acats.log
display ""

display "		=== acats support ==="
display_noeol "Generating support files..."

rm -rf $dir/support
mkdir -p $dir/support
cd $dir/support

cp $testdir/support/*.ada $testdir/support/*.a $testdir/support/*.tst $dir/support

# Find out the size in bit of an address on the target
target_gnatmake $testdir/support/impbit.adb >> $dir/acats.log 2>&1
target_run $dir/support/impbit > $dir/support/impbit.out 2>&1
target_bit=`cat $dir/support/impbit.out`
echo target_bit="$target_bit" >> $dir/acats.log

# Find out a suitable asm statement
# Adapted from configure.ac gcc_cv_as_dwarf2_debug_line
case "$target" in
  ia64*-*-* | s390*-*-*)
    target_insn="nop 0"
    ;;
  mmix-*-*)
    target_insn="swym 0"
    ;;
  *)
    target_insn="nop"
    ;;
esac
echo target_insn="$target_insn" >> $dir/acats.log

sed -e "s,ACATS4GNATDIR,$dir,g" \
  < $testdir/support/impdef.a > $dir/support/impdef.a
sed -e "s,ACATS4GNATDIR,$dir,g" \
  -e "s,ACATS4GNATBIT,$target_bit,g" \
  -e "s,ACATS4GNATINSN,$target_insn,g" \
  < $testdir/support/macro.dfs > $dir/support/MACRO.DFS
sed -e "s,ACATS4GNATDIR,$dir,g" \
  < $testdir/support/tsttests.dat > $dir/support/TSTTESTS.DAT

cp $testdir/tests/cd/*.c $dir/support
cp $testdir/tests/cxb/*.c $dir/support
grep -v '^#' $testdir/norun.lst | sort > $dir/support/norun.lst

rm -rf $dir/run
mv $dir/tests $dir/tests.$$ 2> /dev/null
rm -rf $dir/tests.$$ &
mkdir -p $dir/run

cp -pr $testdir/tests $dir/

for i in $dir/support/*.ada $dir/support/*.a; do 
   host_gnatchop $i >> $dir/acats.log 2>&1
done

# These tools are used to preprocess some ACATS sources
# they need to be compiled native on the host.

host_gnatmake -q -gnatws macrosub.adb
if [ $? -ne 0 ]; then
   display "**** Failed to compile macrosub"
   exit 1
fi
./macrosub > macrosub.out 2>&1

gcc -c cd300051.c
host_gnatmake -q -gnatws widechr.adb
if [ $? -ne 0 ]; then
   display "**** Failed to compile widechr"
   exit 1
fi
./widechr > widechr.out 2>&1

rm -f $dir/support/macrosub
rm -f $dir/support/widechr
rm -f $dir/support/*.ali
rm -f $dir/support/*.o

display " done."

# From here, all compilations will be made by the target compiler

display_noeol "Compiling support files..."

target_gcc -c *.c
if [ $? -ne 0 ]; then
   display "**** Failed to compile C code"
   exit 1
fi

target_gnatchop *.adt >> $dir/acats.log 2>&1

target_gnatmake -c -gnato -gnatE *.ads >> $dir/acats.log 2>&1
target_gnatmake -c -gnato -gnatE *.adb >> $dir/acats.log 2>&1

display " done."
display ""
display "		=== acats tests ==="

if [ $# -eq 0 ]; then
   chapters=`cd $dir/tests; echo [a-z]*`
else
   chapters=$*
fi

glob_countn=0
glob_countok=0
glob_countu=0

for chapter in $chapters; do
   display Running chapter $chapter ...

   if [ ! -d $dir/tests/$chapter ]; then
      display "*** CHAPTER $chapter does not exist, skipping."
      display ""
      continue
   fi

   cd $dir/tests/$chapter
   ls *.a *.ada *.adt *.am *.dep 2> /dev/null | sed -e 's/\(.*\)\..*/\1/g' | \
   cut -c1-7 | sort | uniq | comm -23 - $dir/support/norun.lst \
     > $dir/tests/$chapter/${chapter}.lst 
   countn=`wc -l < $dir/tests/$chapter/${chapter}.lst`
   glob_countn=`expr $glob_countn + $countn`
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
      grep $i $testdir/stackcheck.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -fstack-check"
      fi
      grep $i $testdir/ada95.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -gnat95"
      fi
      test=$dir/tests/$chapter/$i
      mkdir $test && cd $test >> $dir/acats.log 2>&1

      if [ $? -ne 0 ]; then
         display "FAIL:	$i"
         failed="${failed}${i} "
         clean_dir
         continue
      fi

      target_gnatchop -c -w `ls ${test}*.a ${test}*.ada ${test}*.adt ${test}*.am ${test}*.dep 2> /dev/null` >> $dir/acats.log 2>&1
      main=""
      find_main
      if [ -z "$main" ]; then
         sync
         find_main
      fi
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
      if [ ! -x $dir/tests/$chapter/$i/$binmain ]; then
         sync
      fi
      target_run $dir/tests/$chapter/$i/$binmain > $dir/tests/$chapter/$i/${i}.log 2>&1
      cd $dir/tests/$chapter/$i
      cat ${i}.log >> $dir/acats.log
      egrep -e '(==== |\+\+\+\+ |\!\!\!\! )' ${i}.log > /dev/null 2>&1
      if [ $? -ne 0 ]; then
         grep 'tasking not implemented' ${i}.log > /dev/null 2>&1

         if [ $? -ne 0 ]; then
            display "FAIL:	$i"
            failed="${failed}${i} "
         else
            log "UNSUPPORTED:	$i"
            glob_countn=`expr $glob_countn - 1`
            glob_countu=`expr $glob_countu + 1`
         fi
      else
         log "PASS:	$i"
         glob_countok=`expr $glob_countok + 1`
      fi
      clean_dir
   done
done

display "		=== acats Summary ==="
display "# of expected passes		$glob_countok"
display "# of unexpected failures	`expr $glob_countn - $glob_countok`"

if [ $glob_countu -ne 0 ]; then
   display "# of unsupported tests		$glob_countu"
fi

if [ $glob_countok -ne $glob_countn ]; then
   display "*** FAILURES: $failed"
fi

display "$0 completed at `date`"

exit 0
