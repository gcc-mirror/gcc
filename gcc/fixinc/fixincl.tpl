[= autogen template -*- Mode: C -*-
x =]
/*
[= _eval "# * " _DNE =]
 *
 * Install modified versions of certain ANSI-incompatible system header
 * files which are fixed to work correctly with ANSI C and placed in a
 * directory that GNU C will search.
 *
 * This script contains [=_eval fix _hilim 1 +=] fixup scripts.
 *
 * See README-fixinc for more information.
 *
[=_eval inclhack "# *  " _gpl=]
 *[=


_FOR fix =]
 *
 *  Description [=_eval _index 1 + "#%3d -" _printf=] [=hackname _Cap=] fix
 */
tSCC z[=hackname _cap=]Name[] =
     [=hackname _cap _str=];
/*
 *  File name selection pattern
 */[=

  _IF files _exist=]
tSCC z[=hackname _cap=]List[] =[=
    _FOR files=]
        "|" [=files _str=][=
    /files=] "|";[=

  _ELSE =]
#define z[=hackname _cap=]List (char*)NULL[=
  _ENDIF "files _exist" =]
/*
 *  Machine/OS name selection pattern
 */[=

  _IF mach _exist=]
tSCC* apz[=hackname _cap=]Machs[] = {[=
    _FOR mach =]
        [=mach _str=],[=
    /mach=]
        (const char*)NULL };[=

  _ELSE =]
#define apz[=hackname _cap=]Machs (const char**)NULL[=
  _ENDIF "files _exist" =][=

  _IF exesel _exist=]

/*
 *  content selection pattern
 */[=
    _FOR exesel =]
tSCC z[=hackname _cap=]Select[=_eval _index=][] =
       [=exesel _str=];[=
    /exesel =][=

  _ELIF select _exist=]

/*
 *  content selection pattern
 */[=
    _FOR select =]
tSCC z[=hackname _cap=]Select[=_eval _index=][] =
       [=select _str=];[=
    /select =][=
  _ENDIF =][=

  _IF bypass _exist=]

/*
 *  content bypass pattern
 */[=
    _FOR bypass =]
tSCC z[=hackname _cap=]Bypass[=_eval _index=][] =
       [=bypass _str=];[=
    /bypass =][=
  _ENDIF =][=

  _IF test _exist=]

/*
 *  content test pattern.  A shell will deal with it later.
 */[=
    _FOR test =]
tSCC z[=hackname _cap=]Test[=_eval _index=][] =
       [=test _str=];[=
    /test =][=
  _ENDIF =][=

  _IF exesel _exist select _exist bypass _exist test _exist | | |
=]

#define    [=hackname _up =]_TEST_CT  [=
    _IF exesel _exist =][=
       _eval test _count bypass _count exesel _count + + =][=
    _ELSE =][=
       _eval test _count bypass _count select _count + + =][=
    _ENDIF =]
tTestDesc a[=hackname _cap=]Tests[] = {[=

    _IF test _exist =][=
      _FOR test=]
    { TT_TEST,   z[=hackname _cap=]Test[=_eval _index=], 0 /* unused */ },[=
      /test =][=
    _ENDIF =][=

    _IF bypass _exist =][=
      _FOR bypass=]
    { TT_NEGREP, z[=hackname _cap=]Bypass[=_eval _index=], (regex_t*)NULL },[=
      /bypass =][=
    _ENDIF =][=

    _IF exesel _exist =][=
      _FOR exesel ,=]
    { TT_EGREP,  z[=hackname _cap=]Select[=_eval _index=], (regex_t*)NULL }[=
      /exesel =][=

    _ELIF select _exist =][=
      _FOR select ,=]
    { TT_EGREP,  z[=hackname _cap=]Select[=_eval _index=], (regex_t*)NULL }[=
      /select =][=
    _ENDIF =] };[=
  _ELSE =]
#define [=hackname _up=]_TEST_CT  0
#define a[=hackname _cap=]Tests   (tTestDesc*)NULL[=
  _ENDIF =]

/*
 *  Fix Command Arguments for [=hackname _cap=]
 */
const char* apz[=hackname _cap=]Patch[] = {[=
    _IF   sed         _exist =] "sed"[=_FOR sed=],
    "-e" [=sed _str=][=/sed=][=
    _ELIF replacement _exist =] "sed",
    "s@[=select[]=]@[=replacement=]@"[=
    _ELIF shell       _exist =] "sh", "-c",
    [=shell _str=][=
    _ELSE =][=_ERROR hackname _get "Error:  %s has two fixup specifications"
                 _printf =][=
    _ENDIF=],
    (char*)NULL };

/* * * * * * * * * * * * * * * * * * * * * * * * * *[=
/fix=]
 *
 *  List of all fixes
 */
#define  REGEX_COUNT  [=_eval fix.select _count
                              fix.bypass _count + =]
#define  FIX_COUNT    [=_eval fix _count =]
tFixDesc fixDescList[ [=_eval fix _count =] ] = {[=


_FOR fix ",\n" =]
  {  z[=hackname _cap=]Name,    z[=hackname _cap=]List,
     apz[=hackname _cap=]Machs, (regex_t*)NULL,
     [=hackname  _up=]_TEST_CT, [=
       _IF not_machine _exist =]FD_MACH_IFNOT[=
       _ELSE                  =]FD_MACH_ONLY[=
       _ENDIF =],
     a[=hackname _cap=]Tests,   apz[=hackname _cap=]Patch }[=

/fix=]
};
