[= autogen template include =]
[=
# $Id: hackshell.tpl,v 1.2 1998/12/16 21:19:08 law Exp $
#
#  This file contanes the shell template replacement for the
#  fixincl program.  It is the repetitive guts of the fixincludes logic.
#
=]
  if $LINKS; then
    files=`find . -name '*.h' \( -type f -o -type l \) -print`
  else
    files=`find . -name '*.h' -type f -print`
  fi
  echo Checking header files
  for file in $files; do

    if ( test ! -r $file -o \
    -n "`fgrep 'This file is part of the GNU C Library' $file`" )
    then continue ; fi

    fixlist=""
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
    case "$file" in [=_FOR files " | \\\n\t"=]./[=files=][=/files=] )[=
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

       if ( test -n "`egrep 'find-expr' $file`" -a
                 -z "`egrep 'not-find'  $file`" -a
                 '(' <some-test-expression> ')'
          ) > /dev/null 2>&1 ; then

    #  =][=

    _IF select _exist =]
    if ( test [=
        _FOR select " -a \\\n              "
              =]-n [=select _shrstr "#`egrep %s $file`"
                            _printf _shstr =][=
        /select=][=

        _IF bypass _exist =][=
            _FOR bypass=] -a \
              -z [=bypass _shrstr "#`egrep %s $file`"
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
              -z [=bypass _shrstr "#`egrep %s $file`"
                            _printf _shstr=][=
            /bypass=][=
        _ENDIF=]
       ) > /dev/null 2>&1 ; then[=


    _ELIF bypass _exist =]
    if ( test [=_FOR bypass " -a \\\n              "
              =]-z [=bypass _shrstr "#`egrep %s $file`"
                            _printf _shstr=][=/bypass=]
       ) > /dev/null 2>&1 ; then[=

      _ENDIF=]
    fixlist="${fixlist}
      [=hackname=]"
    if [ ! -r ${DESTDIR}/$file ]
    then infile=$file
    else infile=${DESTDIR}/$file ; fi [=

    _IF sed _exist=][=
        _IF shell _exist =][=
          _ERROR hackname _get
          "fixincludes Error:  %s fix has multiple fixups" _printf=][=
        _ENDIF=]

    sed [=
        _FOR sed =]-e [=sed _shrstr=] \
        [=
        /sed=]  < $infile > ${DESTDIR}/$file.[=


    _ELIF shell _exist =]
    ( [=shell=] ) < $infile > ${DESTDIR}/$file.

    #  Shell scripts have the potential of removing the output
    #  We interpret that to mean the file is not to be altered
    #
    if test ! -f ${DESTDIR}/$file.
    then continue ; fi [=


    _ELSE=][=
        _ERROR hackname _get "ERROR:  %s has no fixup" _printf=][=

    _ENDIF=]
    
    mv -f ${DESTDIR}/$file. ${DESTDIR}/$file[=

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
    if ( test ! -f ${DESTDIR}/$file || \
         cmp $file ${DESTDIR}/$file ) > /dev/null 2>&1
    then
      rm -f ${DESTDIR}/$file
    else
      echo "Fixed $file:${fixlist}"

      # Find any include directives that use "file".
      #
      for include in `
         egrep '^[ 	]*#[ 	]*include[ 	]*"[^/]' ${DESTDIR}/$file |
     sed -e 's/^[ 	]*#[ 	]*include[ 	]*"\([^"]*\)".*$/\1/'`
      do
    dir=`echo $file | sed -e s'|/[^/]*$||'`
    required="$required ${SRCDIR} $dir/$include ${DESTDIR}/$dir/$include"
      done
    fi
  done # for file in $files
