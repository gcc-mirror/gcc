[= autogen template include =]
[=
#
#  This file contanes the shell template replacement for the
#  fixincl program.  It is the repetitive guts of the fixincludes logic.
#
=]
  echo Checking header files
  for file in $files; do

    # Skip unreadable files, symlinks to directories and glibc files
    if test ! -r "${file}" || test -d "${file}/." ; then
      continue
    fi

    fixlist=""
    DESTFILE=${DESTDIR}/`echo ${file} | sed "s;${FIND_BASE}/;;" `
[=
#
#  FOR  every fix description,
#  DO:  emit the shell text to apply the fix to the current file
#
# =][=

_FOR fix "\n\n" =]
    #
    # Fix [=
       _IF FIXINC_DEBUG _exist =][=_eval _index 1 + #%3d _printf=]:  [=
       _ENDIF =][=hackname _Cap=]
    #[=
    _IF files _exist=]
    case "${file}" in [=_FOR files " | \\\n\t"=]./[=files=][=/files=] )[=
    _ENDIF=][=

    _IF mach _exist=]
    case "$target_canonical" in [=
        _FOR mach " | \\\n\t" =][=
            mach =][=
        /mach =] )[=
        _IF mach_unmatched _exist =] : ;;
    * )[=
        _ENDIF =][=

    _ENDIF=][=

    #  There are four conditional tests:  select, bypass and test c_test.
       They may appear as often as desired.  They must all pass for
       the fix to be applied.  "select" and "bypass" are egrep expressions
       that must each appear (or not appear) in the target file.
       "test" is an arbitrary test program expression that must yield
       true or false.  =][=

    _IF select _exist =]
    if ( test [=
        _FOR select " -a \\\n              "
              =]-n [=select _shrstr "#`egrep %s ${file}`"
                            _printf _shstr =][=
        /select=]
       ) > /dev/null 2>&1 ; then[=
    _ENDIF =][=

    _IF bypass _exist =]
    if ( test [=
            _FOR bypass " -a \\\n              "
              =]-z [=bypass _shrstr "#`egrep %s ${file}`"
                            _printf _shstr =][=
            /bypass=]
       ) > /dev/null 2>&1 ; then[=
    _ENDIF =][=

    _IF test _exist =]
    if ( test [=
        _FOR test " -a \\\n              "
              =]'(' [=test=] ')'[=
        /test=]
       ) > /dev/null 2>&1 ; then[=
    _ENDIF=][=

    _IF c_test _exist =]
    if [=
        _FOR c_test " && \\\n              "
              =]${FIXTESTS} ${file} [=c_test=][=
        /c_test=]
    then[=

    _ENDIF=][=
    _IF replace _exist ! =]
    fixlist="${fixlist}
      [=hackname=]"
    if [ ! -r ${DESTFILE} ]
    then infile=${file}
    else infile=${DESTFILE} ; fi [=
    _ENDIF =][=

    _IF sed _exist=][=
        _IF shell _exist =][=
          _ERROR hackname _get
          "fixincludes Error:  %s fix has multiple fixups" _printf=][=
        _ENDIF=]

    sed [=
        _FOR sed =]-e [=sed _shrstr=] \
        [=
        /sed=]  < $infile > ${DESTDIR}/fixinc.tmp[=


    _ELIF shell _exist =]
    ( [=shell=] ) < $infile > ${DESTDIR}/fixinc.tmp

    #  Shell scripts have the potential of removing the output
    #  We interpret that to mean the file is not to be altered
    #
    if test ! -f ${DESTDIR}/fixinc.tmp
    then continue ; fi [=

    _ELIF c_fix _exist =]
    ${FIXFIXES} ${file} [=c_fix=] < $infile > ${DESTDIR}/fixinc.tmp[=

    _ELIF replace _exist =][=

      _IF replace _len 0 > =]
    echo "[=hackname =] replacing file ${file}" >&2
    cat > ${DESTFILE} << '_EOF_'
[=replace=]
_EOF_[=
      _ELSE =]
    echo "[=hackname =] bypassing file ${file}"[=
      _ENDIF =]
    continue
[=

    _ELSE=][=
        _ERROR hackname _get "ERROR:  %s has no fixup" _printf=][=

    _ENDIF=][=

    _IF replace _exist ! =]
    rm -f ${DESTFILE}
    mv -f ${DESTDIR}/fixinc.tmp ${DESTFILE}[=
    _ENDIF =][=

    #  Close off any opened "if" or "case" statements in reverse order

    # =][=

    _IF c_test _exist =]
    fi # end of c_test 'if'[=
    _ENDIF =][=

    _IF test _exist =]
    fi # end of test expression 'if'[=
    _ENDIF =][=

    _IF bypass _exist =]
    fi # end of bypass 'if'[=
    _ENDIF =][=

    _IF select _exist =]
    fi # end of select 'if'[=
    _ENDIF =][=

    _IF mach _exist=]
    ;; # case end for machine type test
    esac[=
    _ENDIF =][=

    _IF files _exist=]
    ;; # case end for file name test
    esac[=
    _ENDIF =][=

/fix =][=
#
#  DONE with every fix for the current file
#
#=]
    #  IF the output has been removed OR it is unchanged,
    #  THEN ensure the output is gone
    #  ELSE look for local directory include syntax
    #
    if ( test ! -f ${DESTFILE} || \
         cmp ${file} ${DESTFILE} ) > /dev/null 2>&1
    then
      rm -f ${DESTFILE}
    else
      echo "Fixed ${file}:${fixlist}"

      # Find any include directives that use "file".
      #
      dir=`echo ${file} | sed -e s';/[^/]*$;;'`
      ddir=${DESTDIR}/$dir

      for include in `
         egrep '^[      ]*#[    ]*include[      ]*"[^/]' ${DESTFILE} |
         sed -e 's/^[   ]*#[    ]*include[      ]*"\([^"]*\)".*$/\1/'`
      do
        required="$required ${SRCDIR} $dir/$include ${ddir}/$include"
      done
    fi
  done # for file in $files
