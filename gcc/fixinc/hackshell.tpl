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
    if test ! -r "${file}" || test -d "${file}/." \
       || fgrep 'This file is part of the GNU C Library' "${file}" \
	    > /dev/null 2>&1; then
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
    # Fix [=_eval _index 1 + #%3d _printf=]:  [=hackname _Cap=]
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

    #  There are three conditional tests:  select, bypass and test.
       They may appear as often as desired.  They must all pass for
       the fix to be applied.  "select" and "bypass" are egrep expressions
       that must each appear (or not appear) in the target file.
       "test" is an arbitrary test program expression that must yield
       true or false.  It is enclosed in parenthesis to avoid
       precedence problems.  The output looks like this:

       if ( test -n "`egrep 'find-expr' ${file}`" -a
                 -z "`egrep 'not-find'  ${file}`" -a
                 '(' <some-test-expression> ')'
          ) > /dev/null 2>&1 ; then

    #  =][=

    _IF select _exist =]
    if ( test [=
        _FOR select " -a \\\n              "
              =]-n [=select _shrstr "#`egrep %s ${file}`"
                            _printf _shstr =][=
        /select=][=

        _IF bypass _exist =][=
            _FOR bypass=] -a \
              -z [=bypass _shrstr "#`egrep %s ${file}`"
                            _printf _shstr =][=
            /bypass=][=
        _ENDIF=][=

        _IF test _exist=][=
            _FOR test=] -a \
              '(' [=test=] ')'[=
            /test=][=
        _ENDIF=]
       ) > /dev/null 2>&1 ; then[=


    _ELIF test _exist =]
    if ( test [=
        _FOR test " -a \\\n              "
              =]'(' [=test=] ')'[=
        /test=][=

        _IF bypass _exist=][=
            _FOR bypass=] -a \
              -z [=bypass _shrstr "#`egrep %s ${file}`"
                            _printf _shstr=][=
            /bypass=][=
        _ENDIF=]
       ) > /dev/null 2>&1 ; then[=


    _ELIF bypass _exist =]
    if ( test [=_FOR bypass " -a \\\n              "
              =]-z [=bypass _shrstr "#`egrep %s ${file}`"
                            _printf _shstr=][=/bypass=]
       ) > /dev/null 2>&1 ; then[=

      _ENDIF=]
    fixlist="${fixlist}
      [=hackname=]"
    if [ ! -r ${DESTFILE} ]
    then infile=${file}
    else infile=${DESTFILE} ; fi [=

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


    _ELSE=][=
        _ERROR hackname _get "ERROR:  %s has no fixup" _printf=][=

    _ENDIF=]
    rm -f ${DESTFILE}
    mv -f ${DESTDIR}/fixinc.tmp ${DESTFILE}[=

    #  Close off any opened "if" or "case" statements in reverse order

    # =][=

    _IF select _exist test _exist | bypass _exist | =]
    fi # end of selection 'if'[=
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
