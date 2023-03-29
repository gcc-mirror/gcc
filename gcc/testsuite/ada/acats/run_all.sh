#!/bin/sh
# Run ACATS with the GNU Ada compiler

# The following functions are to be customized if you run in cross
# environment or want to change compilation flags.  Note that for
# tests requiring checks not turned on by default, this script
# automatically adds the needed flags to pass (ie: -gnato or -gnatE).

# gccflags="-O3 -funroll-loops"
# gnatflags="-gnatn"

gccflags="-O2"
gnatflags="-gnatws"

# End of customization section.

# Perform arithmetic evaluation on the ARGs, and store the result in the
# global $as_val. Take advantage of shells that can avoid forks. The arguments
# must be portable across $(()) and expr.
if (eval "test \$(( 1 + 1 )) = 2") 2>/dev/null; then :
  eval 'as_fn_arith ()
  {
    as_val=$(( $* ))
  }'
else
  as_fn_arith ()
  {
    as_val=`expr "$@" || test $? -eq 1`
  }
fi # as_fn_arith

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

if [ "$dir" = "$testdir" ]; then
  echo "error: srcdir must be different than objdir, exiting."
  exit 1
fi

GCC="$BASE/xgcc -B$BASE/"

target_gnatchop () {
  $BASE/gnatchop --GCC="$BASE/xgcc" $*
}

target_gnatmake () {
  echo $BASE/gnatmake --GNATBIND=$BASE/gnatbind --GNATLINK=$BASE/gnatlink --GCC="$GCC" $gnatflags $gccflags $* -largs $EXTERNAL_OBJECTS --GCC="$GCC"
  $BASE/gnatmake --GNATBIND=$BASE/gnatbind --GNATLINK=$BASE/gnatlink --GCC="$GCC" $gnatflags $gccflags $* -largs $EXTERNAL_OBJECTS --GCC="$GCC"
}

target_gcc () {
  $GCC $gccflags $*
}

target_run () {
  eval $EXPECT -f $testdir/run_test.exp $*
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

if [ -n "$GCC_RUNTEST_PARALLELIZE_DIR" ]; then
  dir_support=$dir/../acats/support

  rm -rf $dir/run
  mv $dir/tests $dir/tests.$$ 2> /dev/null
  rm -rf $dir/tests.$$ &
  mkdir -p $dir/run

  cp -pr $dir/../acats/tests $dir/
else
  dir_support=$dir/support

display "		=== acats support ==="
display_noeol "Generating support files..."

rm -rf $dir/support
mkdir -p $dir/support
cd $dir/support

cp $testdir/support/*.ada $testdir/support/*.a $testdir/support/*.tst $dir/support

# Find out the size in bit of an address on the target
target_gnatmake $testdir/support/impbit.adb >> $dir/acats.log 2>&1
if [ $? -ne 0 ]; then
   display "**** Failed to compile impbit"
   exit 1
fi
target_run $dir/support/impbit > $dir/support/impbit.out 2>&1
target_bit=`cat $dir/support/impbit.out`
echo target_bit="$target_bit" >> $dir/acats.log

case "$target_bit" in
  *32*)
    target_max_int="9223372036854775807"
    target_min_int="-9223372036854775808"
    ;;
  *64*)
    target_max_int="170141183460469231731687303715884105727"
    target_min_int="-170141183460469231731687303715884105728"
    ;;
  *)
    display "**** Unsupported bits per word"
    exit 1
esac

echo target_max_insn="$target_max_int" >> $dir/acats.log
echo target_min_insn="$target_min_int" >> $dir/acats.log

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
  -e "s,ACATS4GNATMAXINT,$target_max_int,g" \
  -e "s,ACATS4GNATMININT,$target_min_int,g" \
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

fi

display "		=== acats tests ==="

if [ $# -eq 0 ]; then
   chapters=`cd $dir/tests; echo [a-z]*`
else
   chapters=$*
fi

glob_countn=0
glob_countok=0
glob_countu=0
par_count=0
par_countm=0
par_last=

for chapter in $chapters; do
   # Used to generate support once and finish after that.
   [ "$chapter" = "NONE" ] && continue

   display Running chapter $chapter ...

   if [ ! -d $dir/tests/$chapter ]; then
      display "*** CHAPTER $chapter does not exist, skipping."
      display ""
      continue
   fi

   cd $dir/tests/$chapter
   ls *.a *.ada *.adt *.am *.dep 2> /dev/null | sed -e 's/\(.*\)\..*/\1/g' | \
   cut -c1-7 | sort | uniq | comm -23 - $dir_support/norun.lst \
     > $dir/tests/$chapter/${chapter}.lst 
   countn=`wc -l < $dir/tests/$chapter/${chapter}.lst`
   as_fn_arith $glob_countn + $countn
   glob_countn=$as_val
   for i in `cat $dir/tests/$chapter/${chapter}.lst`; do 

      # If running multiple run_all.sh jobs in parallel, decide
      # if we should run this test in the current instance.
      if [ -n "$GCC_RUNTEST_PARALLELIZE_DIR" ]; then
	 case "$i" in
	    # Ugh, some tests have inter-test dependencies, those
	    # tests have to be scheduled on the same parallel instance
	    # as previous test.
	    ce2108f | ce2108h | ce3112d) ;;
	    # All others can be hopefully scheduled freely.
	    *)
	       as_fn_arith $par_countm + 1
	       par_countm=$as_val
	       [ $par_countm -eq 10 ] && par_countm=0
	       if [ $par_countm -eq 1 ]; then
		  as_fn_arith $par_count + 1
		  par_count=$as_val
		  if mkdir $GCC_RUNTEST_PARALLELIZE_DIR/$par_count 2>/dev/null; then
		     par_last=1
		  else
		     par_last=
		  fi
	       fi;;
	 esac
	 if [ -z "$par_last" ]; then
	    as_fn_arith $glob_countn - 1
	    glob_countn=$as_val
	    continue
	 fi
      fi

      extraflags="-gnat95"
      grep $i $testdir/overflow.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -gnato"
      fi
      grep $i $testdir/elabd.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -gnatE"
      fi
      grep $i $testdir/floatstore.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -ffloat-store"
      fi
      grep $i $testdir/stackcheck.lst > /dev/null 2>&1
      if [ $? -eq 0 ]; then
         extraflags="$extraflags -fstack-check"
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
        cxb30*) EXTERNAL_OBJECTS="$dir_support/cxb30040.o $dir_support/cxb30060.o $dir_support/cxb30130.o $dir_support/cxb30131.o";;
        ca1020e) rm -f ca1020e_func1.adb ca1020e_func2.adb ca1020e_proc1.adb ca1020e_proc2.adb > /dev/null 2>&1;;
        ca14028) rm -f ca14028_func2.ads ca14028_func3.ads ca14028_proc1.ads ca14028_proc3.ads > /dev/null 2>&1;;
      esac
      if [ "$main" = "" ]; then
         display "FAIL:	$i"
         failed="${failed}${i} "
         clean_dir
         continue
      fi

      target_gnatmake $extraflags -I$dir_support $main >> $dir/acats.log 2>&1
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
            as_fn_arith $glob_countn - 1
            glob_countn=$as_val
            as_fn_arith $glob_countu + 1
            glob_countu=$as_val
         fi
      else
         log "PASS:	$i"
         as_fn_arith $glob_countok + 1
         glob_countok=$as_val
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
