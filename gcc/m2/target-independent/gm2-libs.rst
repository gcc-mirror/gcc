@c Copyright (C) 2000-2022 Free Software Foundation, Inc.
@c This file is part of GNU Modula-2.

@c Permission is granted to copy, distribute and/or modify this document
@c under the terms of the GNU Free Documentation License, Version 1.2 or
@c any later version published by the Free Software Foundation.
@menu
* Base libraries::Basic M2F compatible libraries
* PIM and Logitech 3.0 Compatible::PIM and Logitech 3.0 compatible libraries
* PIM coroutine support::PIM compatible process support
* M2 ISO Libraries::ISO defined libraries
@end menu

@c ============================================================

@c @node Base libraries, PIM and Logitech 3.0 Compatible, , Libraries
Base libraries
==============

@menu
* gm2-libs/ASCII::ASCII.def
* gm2-libs/Args::Args.def
* gm2-libs/Assertion::Assertion.def
* gm2-libs/Break::Break.def
* gm2-libs/Builtins::Builtins.def
* gm2-libs/COROUTINES::COROUTINES.def
* gm2-libs/CmdArgs::CmdArgs.def
* gm2-libs/Debug::Debug.def
* gm2-libs/DynamicStrings::DynamicStrings.def
* gm2-libs/Environment::Environment.def
* gm2-libs/FIO::FIO.def
* gm2-libs/FormatStrings::FormatStrings.def
* gm2-libs/FpuIO::FpuIO.def
* gm2-libs/GetOpt::GetOpt.def
* gm2-libs/IO::IO.def
* gm2-libs/Indexing::Indexing.def
* gm2-libs/LMathLib0::LMathLib0.def
* gm2-libs/LegacyReal::LegacyReal.def
* gm2-libs/M2Dependent::M2Dependent.def
* gm2-libs/M2EXCEPTION::M2EXCEPTION.def
* gm2-libs/M2LINK::M2LINK.def
* gm2-libs/M2RTS::M2RTS.def
* gm2-libs/MathLib0::MathLib0.def
* gm2-libs/MemUtils::MemUtils.def
* gm2-libs/NumberIO::NumberIO.def
* gm2-libs/OptLib::OptLib.def
* gm2-libs/PushBackInput::PushBackInput.def
* gm2-libs/RTExceptions::RTExceptions.def
* gm2-libs/RTint::RTint.def
* gm2-libs/SArgs::SArgs.def
* gm2-libs/SCmdArgs::SCmdArgs.def
* gm2-libs/SEnvironment::SEnvironment.def
* gm2-libs/SFIO::SFIO.def
* gm2-libs/SMathLib0::SMathLib0.def
* gm2-libs/SYSTEM::SYSTEM.def
* gm2-libs/Scan::Scan.def
* gm2-libs/Selective::Selective.def
* gm2-libs/StdIO::StdIO.def
* gm2-libs/Storage::Storage.def
* gm2-libs/StrCase::StrCase.def
* gm2-libs/StrIO::StrIO.def
* gm2-libs/StrLib::StrLib.def
* gm2-libs/StringConvert::StringConvert.def
* gm2-libs/SysExceptions::SysExceptions.def
* gm2-libs/SysStorage::SysStorage.def
* gm2-libs/TimeString::TimeString.def
* gm2-libs/UnixArgs::UnixArgs.def
* gm2-libs/cbuiltin::cbuiltin.def
* gm2-libs/cgetopt::cgetopt.def
* gm2-libs/cxxabi::cxxabi.def
* gm2-libs/dtoa::dtoa.def
* gm2-libs/errno::errno.def
* gm2-libs/gdbif::gdbif.def
* gm2-libs/ldtoa::ldtoa.def
* gm2-libs/libc::libc.def
* gm2-libs/libm::libm.def
* gm2-libs/sckt::sckt.def
* gm2-libs/termios::termios.def
* gm2-libs/wrapc::wrapc.def
@end menu

@c @node gm2-libs/ASCII, gm2-libs/Args, , Base libraries
gm2-libs/ASCII
--------------

.. code-block:: modula2
    DEFINITION MODULE ASCII ;

    EXPORT QUALIFIED
         nul, soh, stx, etx, eot, enq, ack, bel,
         bs , ht , nl , vt , np , cr , so , si ,
         dle, dc1, dc2, dc3, dc4, nak, syn, etb,
         can, em , sub, esc, fs , gs , rs , us ,
         sp ,  (* All the above are in order *)
         lf, ff, eof, del, tab, EOL ;
    
    (*
       Note that lf, eof and EOL are added.
    *)
    
    CONST
.. index::
   pair: nul; (const)
   pair: soh; (const)
   pair: stx; (const)
   pair: etx; (const)
.. code-block:: modula2
         nul=000C; soh=001C; stx=002C; etx=003C;
.. index::
   pair: eot; (const)
   pair: enq; (const)
   pair: ack; (const)
   pair: bel; (const)
.. code-block:: modula2
         eot=004C; enq=005C; ack=006C; bel=007C;
.. index::
   pair: bs; (const)
   pair: ht; (const)
   pair: nl; (const)
   pair: vt; (const)
.. code-block:: modula2
         bs =010C; ht =011C; nl =012C; vt =013C;
.. index::
   pair: np; (const)
   pair: cr; (const)
   pair: so; (const)
   pair: si; (const)
.. code-block:: modula2
         np =014C; cr =015C; so =016C; si =017C;
.. index::
   pair: dle; (const)
   pair: dc1; (const)
   pair: dc2; (const)
   pair: dc3; (const)
.. code-block:: modula2
         dle=020C; dc1=021C; dc2=022C; dc3=023C;
.. index::
   pair: dc4; (const)
   pair: nak; (const)
   pair: syn; (const)
   pair: etb; (const)
.. code-block:: modula2
         dc4=024C; nak=025C; syn=026C; etb=027C;
.. index::
   pair: can; (const)
   pair: em; (const)
   pair: sub; (const)
   pair: esc; (const)
.. code-block:: modula2
         can=030C; em =031C; sub=032C; esc=033C;
.. index::
   pair: fs; (const)
   pair: gs; (const)
   pair: rs; (const)
   pair: us; (const)
.. code-block:: modula2
         fs =034C; gs =035C; rs =036C; us =037C;
.. index::
   pair: sp; (const)
.. code-block:: modula2
         sp =040C; (* All the above are in order *)
.. index::
   pair: lf; (const)
   pair: ff; (const)
   pair: eof; (const)
   pair: tab; (const)
.. code-block:: modula2
         lf =nl  ; ff =np  ; eof=eot ; tab=ht  ;
.. index::
   pair: del; (const)
   pair: EOL; (const)
.. code-block:: modula2
         del=177C; EOL=nl  ;
    
    END ASCII.

@c @node gm2-libs/Args, gm2-libs/Assertion, gm2-libs/ASCII, Base libraries
gm2-libs/Args
-------------

.. code-block:: modula2
    DEFINITION MODULE Args ;

    EXPORT QUALIFIED GetArg, Narg ;
    
    
    (*
       GetArg - returns the nth argument from the command line.
                The success of the operation is returned.
    *)
    
.. index::
   GetArg
.. code-block:: modula2
    PROCEDURE GetArg (VAR a: ARRAY OF CHAR; n: CARDINAL) : BOOLEAN ;
    
    
    (*
       Narg - returns the number of arguments available from
              command line.
    *)
    
.. index::
   Narg
.. code-block:: modula2
    PROCEDURE Narg () : CARDINAL ;
    
    
    END Args.

@c @node gm2-libs/Assertion, gm2-libs/Break, gm2-libs/Args, Base libraries
gm2-libs/Assertion
------------------

.. code-block:: modula2
    DEFINITION MODULE Assertion ;

    EXPORT QUALIFIED Assert ;
    
    
    (*
       Assert - tests the boolean Condition, if it fails then HALT
                is called.
    *)
    
.. index::
   Assert
.. code-block:: modula2
    PROCEDURE Assert (Condition: BOOLEAN) ;
    
    
    END Assertion.

@c @node gm2-libs/Break, gm2-libs/Builtins, gm2-libs/Assertion, Base libraries
gm2-libs/Break
--------------

.. code-block:: modula2
    DEFINITION MODULE Break ;

    END Break.

@c @node gm2-libs/Builtins, gm2-libs/COROUTINES, gm2-libs/Break, Base libraries
gm2-libs/Builtins
-----------------

.. code-block:: modula2
    DEFINITION MODULE Builtins ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    (* floating point intrinsic procedure functions *)
    
.. index::
   isfinitef
.. code-block:: modula2
    PROCEDURE __BUILTIN__ isfinitef (x: SHORTREAL) : BOOLEAN ;
.. index::
   isfinite
.. code-block:: modula2
    PROCEDURE __BUILTIN__ isfinite (x: REAL) : BOOLEAN ;
.. index::
   isfinitel
.. code-block:: modula2
    PROCEDURE __BUILTIN__ isfinitel (x: LONGREAL) : BOOLEAN ;
    
.. index::
   sinf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sinf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: REAL) : REAL ;
.. index::
   sinl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sinl (x: LONGREAL) : LONGREAL ;
    
.. index::
   cosf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cosf (x: SHORTREAL) : SHORTREAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: REAL) : REAL ;
.. index::
   cosl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cosl (x: LONGREAL) : LONGREAL ;
    
.. index::
   sqrtf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrtf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: REAL) : REAL ;
.. index::
   sqrtl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrtl (x: LONGREAL) : LONGREAL ;
    
.. index::
   atan2f
.. code-block:: modula2
    PROCEDURE __BUILTIN__ atan2f (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   atan2
.. code-block:: modula2
    PROCEDURE __BUILTIN__ atan2 (x, y: REAL) : REAL ;
.. index::
   atan2l
.. code-block:: modula2
    PROCEDURE __BUILTIN__ atan2l (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   fabsf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ fabsf (x: SHORTREAL) : SHORTREAL ;
.. index::
   fabs
.. code-block:: modula2
    PROCEDURE __BUILTIN__ fabs (x: REAL) : REAL ;
.. index::
   fabsl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ fabsl (x: LONGREAL) : LONGREAL ;
    
.. index::
   logf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ logf (x: SHORTREAL) : SHORTREAL ;
.. index::
   log
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log (x: REAL) : REAL ;
.. index::
   logl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ logl (x: LONGREAL) : LONGREAL ;
    
.. index::
   expf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ expf (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp (x: REAL) : REAL ;
.. index::
   expl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ expl (x: LONGREAL) : LONGREAL ;
    
.. index::
   log10f
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   log10
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log10 (x: REAL) : REAL ;
.. index::
   log10l
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log10l (x: LONGREAL) : LONGREAL ;
    
.. index::
   exp10f
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp10
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp10 (x: REAL) : REAL ;
.. index::
   exp10l
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp10l (x: LONGREAL) : LONGREAL ;
    
.. index::
   ilogbf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ilogbf (x: SHORTREAL) : INTEGER ;
.. index::
   ilogb
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ilogb (x: REAL) : INTEGER ;
.. index::
   ilogbl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ilogbl (x: LONGREAL) : INTEGER ;
    
.. index::
   huge_val
.. code-block:: modula2
    PROCEDURE __BUILTIN__ huge_val () : REAL ;
.. index::
   huge_valf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ huge_valf () : SHORTREAL ;
.. index::
   huge_vall
.. code-block:: modula2
    PROCEDURE __BUILTIN__ huge_vall () : LONGREAL ;
    
.. index::
   significand
.. code-block:: modula2
    PROCEDURE __BUILTIN__ significand (r: REAL) : REAL ;
.. index::
   significandf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ significandf (s: SHORTREAL) : SHORTREAL ;
.. index::
   significandl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ significandl (l: LONGREAL) : LONGREAL ;
    
.. index::
   modf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ modf (x: REAL; VAR y: REAL) : REAL ;
.. index::
   modff
.. code-block:: modula2
    PROCEDURE __BUILTIN__ modff (x: SHORTREAL;
                                 VAR y: SHORTREAL) : SHORTREAL ;
.. index::
   modfl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ modfl (x: LONGREAL; VAR y: LONGREAL) : LONGREAL ;
    
.. index::
   signbit
.. code-block:: modula2
    PROCEDURE __BUILTIN__ signbit (r: REAL) : INTEGER ;
.. index::
   signbitf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ signbitf (s: SHORTREAL) : INTEGER ;
.. index::
   signbitl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ signbitl (l: LONGREAL) : INTEGER ;
    
.. index::
   nextafter
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nextafter (x, y: REAL) : REAL ;
.. index::
   nextafterf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nextafterf (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   nextafterl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nextafterl (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   nexttoward
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nexttoward (x, y: REAL) : LONGREAL ;
.. index::
   nexttowardf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nexttowardf (x, y: SHORTREAL) : LONGREAL ;
.. index::
   nexttowardl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nexttowardl (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   scalb
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalb (x, n: REAL) : REAL ;
.. index::
   scalbf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbf (x, n: SHORTREAL) : SHORTREAL ;
.. index::
   scalbl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbl (x, n: LONGREAL) : LONGREAL ;
    
.. index::
   scalbln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbln (x: REAL; n: LONGINT) : REAL ;
.. index::
   scalblnf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalblnf (x: SHORTREAL; n: LONGINT) : SHORTREAL ;
.. index::
   scalblnl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalblnl (x: LONGREAL; n: LONGINT) : LONGREAL ;
    
.. index::
   scalbn
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbn (x: REAL; n: INTEGER) : REAL ;
.. index::
   scalbnf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbnf (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
.. index::
   scalbnl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbnl (x: LONGREAL; n: INTEGER) : LONGREAL ;
    
    (* complex arithmetic intrincic procedure functions *)
    
.. index::
   cabsf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
.. index::
   cabs
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cabs (z: COMPLEX) : REAL ;
.. index::
   cabsl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cabsl (z: LONGCOMPLEX) : LONGREAL ;
    
.. index::
   cargf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cargf (z: SHORTCOMPLEX) : SHORTREAL ;
.. index::
   carg
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carg (z: COMPLEX) : REAL ;
.. index::
   cargl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cargl (z: LONGCOMPLEX) : LONGREAL ;
    
.. index::
   conjf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   conj
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conj (z: COMPLEX) : COMPLEX ;
.. index::
   conjl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cpowerf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cpowerf (base: SHORTCOMPLEX;
                                   exp: SHORTREAL) : SHORTCOMPLEX ;
.. index::
   cpower
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cpower (base: COMPLEX; exp: REAL) : COMPLEX ;
.. index::
   cpowerl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cpowerl (base: LONGCOMPLEX;
                                   exp: LONGREAL) : LONGCOMPLEX ;
    
.. index::
   csqrtf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   csqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csqrt (z: COMPLEX) : COMPLEX ;
.. index::
   csqrtl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cexpf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   cexp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cexp (z: COMPLEX) : COMPLEX ;
.. index::
   cexpl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   clnf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ clnf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   cln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cln (z: COMPLEX) : COMPLEX ;
.. index::
   clnl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ clnl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   csinf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   csin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csin (z: COMPLEX) : COMPLEX ;
.. index::
   csinl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   ccosf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   ccos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ccos (z: COMPLEX) : COMPLEX ;
.. index::
   ccosl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   ctanf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   ctan
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ctan (z: COMPLEX) : COMPLEX ;
.. index::
   ctanl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   carcsinf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carcsinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   carcsin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carcsin (z: COMPLEX) : COMPLEX ;
.. index::
   carcsinl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carcsinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   carccosf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   carccos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carccos (z: COMPLEX) : COMPLEX ;
.. index::
   carccosl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   carctanf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   carctan
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carctan (z: COMPLEX) : COMPLEX ;
.. index::
   carctanl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
    (* memory and string intrincic procedure functions *)
    
.. index::
   alloca
.. code-block:: modula2
    PROCEDURE __BUILTIN__ alloca (i: CARDINAL) : ADDRESS ;
.. index::
   memcpy
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memcpy (dest, src: ADDRESS;
                                  nbytes: CARDINAL) : ADDRESS ;
.. index::
   index
.. code-block:: modula2
    PROCEDURE __BUILTIN__ index (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   rindex
.. code-block:: modula2
    PROCEDURE __BUILTIN__ rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   memcmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memcmp (s1, s2: ADDRESS;
                                  nbytes: CARDINAL) : INTEGER ;
.. index::
   memset
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memset (s: ADDRESS; c: INTEGER;
                                  nbytes: CARDINAL) : ADDRESS ;
.. index::
   memmove
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memmove (s1, s2: ADDRESS;
                                   nbytes: CARDINAL) : ADDRESS ;
.. index::
   strcat
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcat (dest, src: ADDRESS) : ADDRESS ;
.. index::
   strncat
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strncat (dest, src: ADDRESS;
                                   nbytes: CARDINAL) : ADDRESS ;
.. index::
   strcpy
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcpy (dest, src: ADDRESS) : ADDRESS ;
.. index::
   strncpy
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strncpy (dest, src: ADDRESS;
                                   nbytes: CARDINAL) : ADDRESS ;
.. index::
   strcmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcmp (s1, s2: ADDRESS) : INTEGER ;
.. index::
   strncmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strncmp (s1, s2: ADDRESS;
                                   nbytes: CARDINAL) : INTEGER ;
.. index::
   strlen
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strlen (s: ADDRESS) : INTEGER ;
.. index::
   strstr
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strstr (haystack, needle: ADDRESS) : ADDRESS ;
.. index::
   strpbrk
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strpbrk (s, accept: ADDRESS) : ADDRESS ;
.. index::
   strspn
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strspn (s, accept: ADDRESS) : CARDINAL ;
.. index::
   strcspn
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcspn (s, accept: ADDRESS) : CARDINAL ;
.. index::
   strchr
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   strrchr
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
    
    (*
       longjmp - this GCC builtin restricts the val to always 1.
    *)
    (* do not use these two builtins, as gcc, only really
       anticipates that the Ada front end should use them
       and it only uses them in its runtime exception handling.
       We leave them here in the hope that someday they will
       behave more like their libc counterparts.  *)
    
.. index::
   longjmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ longjmp (env: ADDRESS; val: INTEGER) ;
.. index::
   setjmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ setjmp (env: ADDRESS) : INTEGER ;
    
    
    (*
       frame_address - returns the address of the frame.
                       The current frame is obtained if level is 0,
                       the next level up if level is 1 etc.
    *)
    
.. index::
   frame_address
.. code-block:: modula2
    PROCEDURE __BUILTIN__ frame_address (level: CARDINAL) : ADDRESS ;
    
    
    (*
       return_address - returns the return address of function.
                        The current function return address is
                        obtained if level is 0,
                        the next level up if level is 1 etc.
    *)
    
.. index::
   return_address
.. code-block:: modula2
    PROCEDURE __BUILTIN__ return_address (level: CARDINAL) : ADDRESS ;
    
    
    (*
       alloca_trace - this is a no-op which is used for internal debugging.
    *)
    
.. index::
   alloca_trace
.. code-block:: modula2
    PROCEDURE alloca_trace (returned: ADDRESS; nBytes: CARDINAL) : ADDRESS ;
    
    
    END Builtins.

@c @node gm2-libs/COROUTINES, gm2-libs/CmdArgs, gm2-libs/Builtins, Base libraries
gm2-libs/COROUTINES
-------------------

.. code-block:: modula2
    DEFINITION MODULE FOR "C" COROUTINES ;

    CONST
       UnassignedPriority = 0 ;
    
    TYPE
.. index::
   pair: INTERRUPTSOURCE; (type)
.. code-block:: modula2
       INTERRUPTSOURCE = CARDINAL ;
.. index::
   pair: PROTECTION; (type)
.. code-block:: modula2
       PROTECTION = [UnassignedPriority..7] ;
    
    END COROUTINES.

@c @node gm2-libs/CmdArgs, gm2-libs/Debug, gm2-libs/COROUTINES, Base libraries
gm2-libs/CmdArgs
----------------

.. code-block:: modula2
    DEFINITION MODULE CmdArgs ;

    EXPORT QUALIFIED GetArg, Narg ;
    
    
    (*
       GetArg - returns the nth argument from the command line, CmdLine
                the success of the operation is returned.
    *)
    
.. index::
   GetArg
.. code-block:: modula2
    PROCEDURE GetArg (CmdLine: ARRAY OF CHAR;
                      n: CARDINAL; VAR Argi: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       Narg - returns the number of arguments available from
              command line, CmdLine.
    *)
    
.. index::
   Narg
.. code-block:: modula2
    PROCEDURE Narg (CmdLine: ARRAY OF CHAR) : CARDINAL ;
    
    
    END CmdArgs.

@c @node gm2-libs/Debug, gm2-libs/DynamicStrings, gm2-libs/CmdArgs, Base libraries
gm2-libs/Debug
--------------

.. code-block:: modula2
    DEFINITION MODULE Debug ;

(*
    Description: provides some simple debugging routines.
*)
    
    EXPORT QUALIFIED Halt, DebugString ;
    
    
    (*
       Halt - writes a message in the format:
              Module:Line:Message
    
              It then terminates by calling HALT.
    *)
    
.. index::
   Halt
.. code-block:: modula2
    PROCEDURE Halt (Message: ARRAY OF CHAR;
                    LineNo: CARDINAL;
                    Module: ARRAY OF CHAR) ;
    
    
    (*
       DebugString - writes a string to the debugging device (Scn.Write).
                     It interprets \n as carriage return, linefeed.
    *)
    
.. index::
   DebugString
.. code-block:: modula2
    PROCEDURE DebugString (a: ARRAY OF CHAR) ;
    
    
    END Debug.

@c @node gm2-libs/DynamicStrings, gm2-libs/Environment, gm2-libs/Debug, Base libraries
gm2-libs/DynamicStrings
-----------------------

.. code-block:: modula2
    DEFINITION MODULE DynamicStrings ;

    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT QUALIFIED String,
                     InitString, KillString, Fin, InitStringCharStar,
                     InitStringChar, Index, RIndex,
                     Mark, Length, ConCat, ConCatChar, Assign, Dup, Add,
                     Equal, EqualCharStar, EqualArray, ToUpper, ToLower,
                     CopyOut, Mult, Slice,
                     RemoveWhitePrefix, RemoveWhitePostfix, RemoveComment,
                     char, string,
                     InitStringDB, InitStringCharStarDB, InitStringCharDB,
                     MultDB, DupDB, SliceDB,
                     PushAllocation, PopAllocation, PopAllocationExemption ;
    
    TYPE
.. index::
   pair: String; (type)
.. code-block:: modula2
       String ;
    
    
    (*
       InitString - creates and returns a String type object.
                    Initial contents are, a.
    *)
    
.. index::
   InitString
.. code-block:: modula2
    PROCEDURE InitString (a: ARRAY OF CHAR) : String ;
    
    
    (*
       KillString - frees String, s, and its contents.
                    NIL is returned.
    *)
    
.. index::
   KillString
.. code-block:: modula2
    PROCEDURE KillString (s: String) : String ;
    
    
    (*
       Fin - finishes with a string, it calls KillString with, s.
             The purpose of the procedure is to provide a short cut
             to calling KillString and then testing the return result.
    *)
    
.. index::
   Fin
.. code-block:: modula2
    PROCEDURE Fin (s: String) ;
    
    
    (*
       InitStringCharStar - initializes and returns a String to contain
                            the C string.
    *)
    
.. index::
   InitStringCharStar
.. code-block:: modula2
    PROCEDURE InitStringCharStar (a: ADDRESS) : String ;
    
    
    (*
       InitStringChar - initializes and returns a String to contain the
                        single character, ch.
    *)
    
.. index::
   InitStringChar
.. code-block:: modula2
    PROCEDURE InitStringChar (ch: CHAR) : String ;
    
    
    (*
       Mark - marks String, s, ready for garbage collection.
    *)
    
.. index::
   Mark
.. code-block:: modula2
    PROCEDURE Mark (s: String) : String ;
    
    
    (*
       Length - returns the length of the String, s.
    *)
    
.. index::
   Length
.. code-block:: modula2
    PROCEDURE Length (s: String) : CARDINAL ;
    
    
    (*
       ConCat - returns String, a, after the contents of, b,
                have been appended.
    *)
    
.. index::
   ConCat
.. code-block:: modula2
    PROCEDURE ConCat (a, b: String) : String ;
    
    
    (*
       ConCatChar - returns String, a, after character, ch,
                    has been appended.
    *)
    
.. index::
   ConCatChar
.. code-block:: modula2
    PROCEDURE ConCatChar (a: String; ch: CHAR) : String ;
    
    
    (*
       Assign - assigns the contents of, b, into, a.
                String, a, is returned.
    *)
    
.. index::
   Assign
.. code-block:: modula2
    PROCEDURE Assign (a, b: String) : String ;
    
    
    (*
       Dup - duplicate a String, s, returning the copy of s.
    *)
    
.. index::
   Dup
.. code-block:: modula2
    PROCEDURE Dup (s: String) : String ;
    
    
    (*
       Add - returns a new String which contains the contents of a and b.
    *)
    
.. index::
   Add
.. code-block:: modula2
    PROCEDURE Add (a, b: String) : String ;
    
    
    (*
       Equal - returns TRUE if String, a, and, b, are equal.
    *)
    
.. index::
   Equal
.. code-block:: modula2
    PROCEDURE Equal (a, b: String) : BOOLEAN ;
    
    
    (*
       EqualCharStar - returns TRUE if contents of String, s, is
                       the same as the string, a.
    *)
    
.. index::
   EqualCharStar
.. code-block:: modula2
    PROCEDURE EqualCharStar (s: String; a: ADDRESS) : BOOLEAN ;
    
    
    (*
       EqualArray - returns TRUE if contents of String, s, is the
                    same as the string, a.
    *)
    
.. index::
   EqualArray
.. code-block:: modula2
    PROCEDURE EqualArray (s: String; a: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       Mult - returns a new string which is n concatenations of String, s.
              If n<=0 then an empty string is returned.
    *)
    
.. index::
   Mult
.. code-block:: modula2
    PROCEDURE Mult (s: String; n: CARDINAL) : String ;
    
    
    (*
       Slice - returns a new string which contains the elements
               low..high-1
    
               strings start at element 0
               Slice(s, 0, 2)  will return elements 0, 1 but not 2
               Slice(s, 1, 3)  will return elements 1, 2 but not 3
               Slice(s, 2, 0)  will return elements 2..max
               Slice(s, 3, -1) will return elements 3..max-1
               Slice(s, 4, -2) will return elements 4..max-2
    *)
    
.. index::
   Slice
.. code-block:: modula2
    PROCEDURE Slice (s: String; low, high: INTEGER) : String ;
    
    
    (*
       Index - returns the indice of the first occurance of, ch, in
               String, s. -1 is returned if, ch, does not exist.
               The search starts at position, o.
    *)
    
.. index::
   Index
.. code-block:: modula2
    PROCEDURE Index (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;
    
    
    (*
       RIndex - returns the indice of the last occurance of, ch,
                in String, s. The search starts at position, o.
                -1 is returned if, ch, is not found.
    *)
    
.. index::
   RIndex
.. code-block:: modula2
    PROCEDURE RIndex (s: String; ch: CHAR; o: CARDINAL) : INTEGER ;
    
    
    (*
       RemoveComment - assuming that, comment, is a comment delimiter
                       which indicates anything to its right is a comment
                       then strip off the comment and also any white space
                       on the remaining right hand side.
                       It leaves any white space on the left hand side
                       alone.
    *)
    
.. index::
   RemoveComment
.. code-block:: modula2
    PROCEDURE RemoveComment (s: String; comment: CHAR) : String ;
    
    
    (*
       RemoveWhitePrefix - removes any leading white space from String, s.
                           A new string is returned.
    *)
    
.. index::
   RemoveWhitePrefix
.. code-block:: modula2
    PROCEDURE RemoveWhitePrefix (s: String) : String ;
    
    
    (*
       RemoveWhitePostfix - removes any leading white space from String, s.
                            A new string is returned.
    *)
    
.. index::
   RemoveWhitePostfix
.. code-block:: modula2
    PROCEDURE RemoveWhitePostfix (s: String) : String ;
    
    
    (*
       ToUpper - returns string, s, after it has had its lower case
                 characters replaced by upper case characters.
                 The string, s, is not duplicated.
    *)
    
.. index::
   ToUpper
.. code-block:: modula2
    PROCEDURE ToUpper (s: String) : String ;
    
    
    (*
       ToLower - returns string, s, after it has had its upper case
                 characters replaced by lower case characters.
                 The string, s, is not duplicated.
    *)
    
.. index::
   ToLower
.. code-block:: modula2
    PROCEDURE ToLower (s: String) : String ;
    
    
    (*
       CopyOut - copies string, s, to a.
    *)
    
.. index::
   CopyOut
.. code-block:: modula2
    PROCEDURE CopyOut (VAR a: ARRAY OF CHAR; s: String) ;
    
    
    (*
       char - returns the character, ch, at position, i, in String, s.
              As Slice the index can be negative so:
    
              char(s, 0) will return the first character
              char(s, 1) will return the second character
              char(s, -1) will return the last character
              char(s, -2) will return the penultimate character
    
              a nul character is returned if the index is out of range.
    *)
    
.. index::
   char
.. code-block:: modula2
    PROCEDURE char (s: String; i: INTEGER) : CHAR ;
    
    
    (*
       string - returns the C style char * of String, s.
    *)
    
.. index::
   string
.. code-block:: modula2
    PROCEDURE string (s: String) : ADDRESS ;
    
    
    (*
       to easily debug an application using this library one could use
       use the following macro processing defines:
    
       #define InitString(X) InitStringDB(X, __FILE__, __LINE__)
       #define InitStringCharStar(X) InitStringCharStarDB(X, \
         __FILE__, __LINE__)
       #define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
       #define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
       #define Dup(X) DupDB(X, __FILE__, __LINE__)
       #define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
    
       and then invoke gm2 with the -fcpp flag.
    *)
    
    
    (*
       InitStringDB - the debug version of InitString.
    *)
    
.. index::
   InitStringDB
.. code-block:: modula2
    PROCEDURE InitStringDB (a: ARRAY OF CHAR;
                            file: ARRAY OF CHAR; line: CARDINAL) : String ;
    
    
    (*
       InitStringCharStarDB - the debug version of InitStringCharStar.
    *)
    
.. index::
   InitStringCharStarDB
.. code-block:: modula2
    PROCEDURE InitStringCharStarDB (a: ADDRESS;
                                    file: ARRAY OF CHAR;
                                    line: CARDINAL) : String ;
    
    
    (*
       InitStringCharDB - the debug version of InitStringChar.
    *)
    
.. index::
   InitStringCharDB
.. code-block:: modula2
    PROCEDURE InitStringCharDB (ch: CHAR;
                                file: ARRAY OF CHAR;
                                line: CARDINAL) : String ;
    
    
    (*
       MultDB - the debug version of MultDB.
    *)
    
.. index::
   MultDB
.. code-block:: modula2
    PROCEDURE MultDB (s: String; n: CARDINAL;
                      file: ARRAY OF CHAR; line: CARDINAL) : String ;
    
    
    (*
       DupDB - the debug version of Dup.
    *)
    
.. index::
   DupDB
.. code-block:: modula2
    PROCEDURE DupDB (s: String;
                     file: ARRAY OF CHAR; line: CARDINAL) : String ;
    
    
    (*
       SliceDB - debug version of Slice.
    *)
    
.. index::
   SliceDB
.. code-block:: modula2
    PROCEDURE SliceDB (s: String; low, high: INTEGER;
                       file: ARRAY OF CHAR; line: CARDINAL) : String ;
    
    (*
       PushAllocation - pushes the current allocation/deallocation lists.
    *)
    
.. index::
   PushAllocation
.. code-block:: modula2
    PROCEDURE PushAllocation ;
    
    
    (*
       PopAllocation - test to see that all strings are deallocated since
                       the last push.  Then it pops to the previous
                       allocation/deallocation lists.
    
                       If halt is true then the application terminates
                       with an exit code of 1.
    *)
    
.. index::
   PopAllocation
.. code-block:: modula2
    PROCEDURE PopAllocation (halt: BOOLEAN) ;
    
    
    (*
       PopAllocationExemption - test to see that all strings are
                                deallocated, except string, e, since
                                the last push.
                                Then it pops to the previous
                                allocation/deallocation lists.
    
                                If halt is true then the application
                                terminates with an exit code of 1.
    
                                The string, e, is returned unmodified,
    *)
    
.. index::
   PopAllocationExemption
.. code-block:: modula2
    PROCEDURE PopAllocationExemption (halt: BOOLEAN; e: String) : String ;
    
    
    END DynamicStrings.

@c @node gm2-libs/Environment, gm2-libs/FIO, gm2-libs/DynamicStrings, Base libraries
gm2-libs/Environment
--------------------

.. code-block:: modula2
    DEFINITION MODULE Environment ;

    EXPORT QUALIFIED GetEnvironment, PutEnvironment ;
    
    
    (*
       GetEnvironment - gets the environment variable Env and places
          	       	    a copy of its value into string, dest.
                        It returns TRUE if the string Env was found in
                        the processes environment.
    *)
    
.. index::
   GetEnvironment
.. code-block:: modula2
    PROCEDURE GetEnvironment (Env: ARRAY OF CHAR;
                              VAR dest: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       PutEnvironment - change or add an environment variable definition
                        EnvDef.
                        TRUE is returned if the environment variable was
                        set or changed successfully.
    *)
    
.. index::
   PutEnvironment
.. code-block:: modula2
    PROCEDURE PutEnvironment (EnvDef: ARRAY OF CHAR) : BOOLEAN ;
    
    
    END Environment.

@c @node gm2-libs/FIO, gm2-libs/FormatStrings, gm2-libs/Environment, Base libraries
gm2-libs/FIO
------------

.. code-block:: modula2
    DEFINITION MODULE FIO ;

(* Provides a simple buffered file input/output library.  *)
    
    
    FROM SYSTEM IMPORT ADDRESS, BYTE ;
    
    EXPORT QUALIFIED (* types *)
                     File,
                     (* procedures *)
                     OpenToRead, OpenToWrite, OpenForRandom, Close,
                     EOF, EOLN, WasEOLN, IsNoError, Exists, IsActive,
                     exists, openToRead, openToWrite, openForRandom,
                     SetPositionFromBeginning,
                     SetPositionFromEnd,
                     FindPosition,
                     ReadChar, ReadString,
                     WriteChar, WriteString, WriteLine,
                     WriteCardinal, ReadCardinal,
                     UnReadChar,
                     WriteNBytes, ReadNBytes,
                     FlushBuffer,
                     GetUnixFileDescriptor,
                     GetFileName, getFileName, getFileNameLength,
                     FlushOutErr,
                     (* variables *)
                     StdIn, StdOut, StdErr ;
    
    TYPE
.. index::
   pair: File; (type)
.. code-block:: modula2
       File = CARDINAL ;
    
    (* the following variables are initialized to their UNIX equivalents *)
    VAR
.. index::
   pair: StdIn; (var)
   pair: StdOut; (var)
   pair: StdErr; (var)
.. code-block:: modula2
       StdIn, StdOut, StdErr: File ;
    
    
    
    (*
       IsNoError - returns a TRUE if no error has occured on file, f.
    *)
    
.. index::
   IsNoError
.. code-block:: modula2
    PROCEDURE IsNoError (f: File) : BOOLEAN ;
    
    
    (*
       IsActive - returns TRUE if the file, f, is still active.
    *)
    
.. index::
   IsActive
.. code-block:: modula2
    PROCEDURE IsActive (f: File) : BOOLEAN ;
    
    
    (*
       Exists - returns TRUE if a file named, fname exists for reading.
    *)
    
.. index::
   Exists
.. code-block:: modula2
    PROCEDURE Exists (fname: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       OpenToRead - attempts to open a file, fname, for reading and
                    it returns this file.
                    The success of this operation can be checked by
                    calling IsNoError.
    *)
    
.. index::
   OpenToRead
.. code-block:: modula2
    PROCEDURE OpenToRead (fname: ARRAY OF CHAR) : File ;
    
    
    (*
       OpenToWrite - attempts to open a file, fname, for write and
                     it returns this file.
                     The success of this operation can be checked by
                     calling IsNoError.
    *)
    
.. index::
   OpenToWrite
.. code-block:: modula2
    PROCEDURE OpenToWrite (fname: ARRAY OF CHAR) : File ;
    
    
    (*
       OpenForRandom - attempts to open a file, fname, for random access
                       read or write and it returns this file.
                       The success of this operation can be checked by
                       calling IsNoError.
                       towrite, determines whether the file should be
                       opened for writing or reading.
                       newfile, determines whether a file should be
                       created if towrite is TRUE or whether the
                       previous file should be left alone,
                       allowing this descriptor to seek
                       and modify an existing file.
    *)
    
.. index::
   OpenForRandom
.. code-block:: modula2
    PROCEDURE OpenForRandom (fname: ARRAY OF CHAR;
                             towrite, newfile: BOOLEAN) : File ;
    
    
    (*
       Close - close a file which has been previously opened using:
               OpenToRead, OpenToWrite, OpenForRandom.
               It is correct to close a file which has an error status.
    *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (f: File) ;
    
    
    (* the following functions are functionally equivalent to the above
       except they allow C style names.
    *)
    
.. index::
   exists
.. code-block:: modula2
    PROCEDURE exists        (fname: ADDRESS; flength: CARDINAL) : BOOLEAN ;
.. index::
   openToRead
.. code-block:: modula2
    PROCEDURE openToRead    (fname: ADDRESS; flength: CARDINAL) : File ;
.. index::
   openToWrite
.. code-block:: modula2
    PROCEDURE openToWrite   (fname: ADDRESS; flength: CARDINAL) : File ;
.. index::
   openForRandom
.. code-block:: modula2
    PROCEDURE openForRandom (fname: ADDRESS; flength: CARDINAL;
                             towrite, newfile: BOOLEAN) : File ;
    
    
    (*
       FlushBuffer - flush contents of the FIO file, f, to libc.
    *)
    
.. index::
   FlushBuffer
.. code-block:: modula2
    PROCEDURE FlushBuffer (f: File) ;
    
    
    (*
       ReadNBytes - reads nBytes of a file into memory area, dest, returning
                    the number of bytes actually read.
                    This function will consume from the buffer and then
                    perform direct libc reads. It is ideal for large reads.
    *)
    
.. index::
   ReadNBytes
.. code-block:: modula2
    PROCEDURE ReadNBytes (f: File; nBytes: CARDINAL;
                          dest: ADDRESS) : CARDINAL ;
    
    
    (*
       ReadAny - reads HIGH(a) bytes into, a. All input
                 is fully buffered, unlike ReadNBytes and thus is more
                 suited to small reads.
    *)
    
.. index::
   ReadAny
.. code-block:: modula2
    PROCEDURE ReadAny (f: File; VAR a: ARRAY OF BYTE) ;
    
    
    (*
       WriteNBytes - writes nBytes from memory area src to a file
                     returning the number of bytes actually written.
                     This function will flush the buffer and then
                     write the nBytes using a direct write from libc.
                     It is ideal for large writes.
    *)
    
.. index::
   WriteNBytes
.. code-block:: modula2
    PROCEDURE WriteNBytes (f: File; nBytes: CARDINAL;
                           src: ADDRESS) : CARDINAL ;
    
    
    (*
       WriteAny - writes HIGH(a) bytes onto, file, f. All output
                  is fully buffered, unlike WriteNBytes and thus is more
                  suited to small writes.
    *)
    
.. index::
   WriteAny
.. code-block:: modula2
    PROCEDURE WriteAny (f: File; VAR a: ARRAY OF BYTE) ;
    
    
    (*
       WriteChar - writes a single character to file, f.
    *)
    
.. index::
   WriteChar
.. code-block:: modula2
    PROCEDURE WriteChar (f: File; ch: CHAR) ;
    
    
    (*
       EOF - tests to see whether a file, f, has reached end of file.
    *)
    
.. index::
   EOF
.. code-block:: modula2
    PROCEDURE EOF (f: File) : BOOLEAN ;
    
    
    (*
       EOLN - tests to see whether a file, f, is about to read a newline.
              It does NOT consume the newline.  It reads the next character
              and then immediately unreads the character.
    *)
    
.. index::
   EOLN
.. code-block:: modula2
    PROCEDURE EOLN (f: File) : BOOLEAN ;
    
    
    (*
       WasEOLN - tests to see whether a file, f, has just read a newline
                 character.
    *)
    
.. index::
   WasEOLN
.. code-block:: modula2
    PROCEDURE WasEOLN (f: File) : BOOLEAN ;
    
    
    (*
       ReadChar - returns a character read from file, f.
                  Sensible to check with IsNoError or EOF after calling
                  this function.
    *)
    
.. index::
   ReadChar
.. code-block:: modula2
    PROCEDURE ReadChar (f: File) : CHAR ;
    
    
    (*
       UnReadChar - replaces a character, ch, back into file, f.
                    This character must have been read by ReadChar
                    and it does not allow successive calls.  It may
                    only be called if the previous read was successful,
                    end of file or end of line seen.
    *)
    
.. index::
   UnReadChar
.. code-block:: modula2
    PROCEDURE UnReadChar (f: File ; ch: CHAR) ;
    
    
    (*
       WriteLine - writes out a linefeed to file, f.
    *)
    
.. index::
   WriteLine
.. code-block:: modula2
    PROCEDURE WriteLine (f: File) ;
    
    
    (*
       WriteString - writes a string to file, f.
    *)
    
.. index::
   WriteString
.. code-block:: modula2
    PROCEDURE WriteString (f: File; a: ARRAY OF CHAR) ;
    
    
    (*
       ReadString - reads a string from file, f, into string, a.
                    It terminates the string if HIGH is reached or
                    if a newline is seen or an error occurs.
    *)
    
.. index::
   ReadString
.. code-block:: modula2
    PROCEDURE ReadString (f: File; VAR a: ARRAY OF CHAR) ;
    
    
    (*
       WriteCardinal - writes a CARDINAL to file, f.
                       It writes the binary image of the CARDINAL.
                       to file, f.
    *)
    
.. index::
   WriteCardinal
.. code-block:: modula2
    PROCEDURE WriteCardinal (f: File; c: CARDINAL) ;
    
    
    (*
       ReadCardinal - reads a CARDINAL from file, f.
                      It reads a bit image of a CARDINAL
                      from file, f.
    *)
    
.. index::
   ReadCardinal
.. code-block:: modula2
    PROCEDURE ReadCardinal (f: File) : CARDINAL ;
    
    
    (*
       GetUnixFileDescriptor - returns the UNIX file descriptor of a file.
                               Useful when combining FIO.mod with select
                               (in Selective.def - but note the comments in
                                Selective about using read/write primatives)
    *)
    
.. index::
   GetUnixFileDescriptor
.. code-block:: modula2
    PROCEDURE GetUnixFileDescriptor (f: File) : INTEGER ;
    
    
    (*
       SetPositionFromBeginning - sets the position from the beginning
                                  of the file.
    *)
    
.. index::
   SetPositionFromBeginning
.. code-block:: modula2
    PROCEDURE SetPositionFromBeginning (f: File; pos: LONGINT) ;
    
    
    (*
       SetPositionFromEnd - sets the position from the end of the file.
    *)
    
.. index::
   SetPositionFromEnd
.. code-block:: modula2
    PROCEDURE SetPositionFromEnd (f: File; pos: LONGINT) ;
    
    
    (*
       FindPosition - returns the current absolute position in file, f.
    *)
    
.. index::
   FindPosition
.. code-block:: modula2
    PROCEDURE FindPosition (f: File) : LONGINT ;
    
    
    (*
       GetFileName - assigns, a, with the filename associated with, f.
    *)
    
.. index::
   GetFileName
.. code-block:: modula2
    PROCEDURE GetFileName (f: File; VAR a: ARRAY OF CHAR) ;
    
    
    (*
       getFileName - returns the address of the filename associated with, f.
    *)
    
.. index::
   getFileName
.. code-block:: modula2
    PROCEDURE getFileName (f: File) : ADDRESS ;
    
    
    (*
       getFileNameLength - returns the number of characters associated with
                           filename, f.
    *)
    
.. index::
   getFileNameLength
.. code-block:: modula2
    PROCEDURE getFileNameLength (f: File) : CARDINAL ;
    
    
    (*
       FlushOutErr - flushes, StdOut, and, StdErr.
    *)
    
.. index::
   FlushOutErr
.. code-block:: modula2
    PROCEDURE FlushOutErr ;
    
    
    END FIO.

@c @node gm2-libs/FormatStrings, gm2-libs/FpuIO, gm2-libs/FIO, Base libraries
gm2-libs/FormatStrings
----------------------

.. code-block:: modula2
    DEFINITION MODULE FormatStrings ;

    FROM SYSTEM IMPORT BYTE ;
    FROM DynamicStrings IMPORT String ;
    EXPORT QUALIFIED Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4,
                     HandleEscape ;
    
    
    (*
       Sprintf0 - returns a String containing, fmt, after it has had its
                  escape sequences translated.
    *)
    
.. index::
   Sprintf0
.. code-block:: modula2
    PROCEDURE Sprintf0 (fmt: String) : String ;
    
    
    (*
       Sprintf1 - returns a String containing, fmt, together with
                  encapsulated entity, w. It only formats the
                  first %s or %d with n.
    *)
    
.. index::
   Sprintf1
.. code-block:: modula2
    PROCEDURE Sprintf1 (fmt: String; w: ARRAY OF BYTE) : String ;
    
    
    (*
       Sprintf2 - returns a string, fmt, which has been formatted.
    *)
    
.. index::
   Sprintf2
.. code-block:: modula2
    PROCEDURE Sprintf2 (fmt: String; w1, w2: ARRAY OF BYTE) : String ;
    
    
    (*
       Sprintf3 - returns a string, fmt, which has been formatted.
    *)
    
.. index::
   Sprintf3
.. code-block:: modula2
    PROCEDURE Sprintf3 (fmt: String; w1, w2, w3: ARRAY OF BYTE) : String ;
    
    
    (*
       Sprintf4 - returns a string, fmt, which has been formatted.
    *)
    
.. index::
   Sprintf4
.. code-block:: modula2
    PROCEDURE Sprintf4 (fmt: String;
                        w1, w2, w3, w4: ARRAY OF BYTE) : String ;
    
    
    (*
       HandleEscape - translates \a, \b, \e, \f, \n, \r, \x[hex] \[octal]
                      into their respective ascii codes.  It also converts
                      \[any] into a single [any] character.
    *)
    
.. index::
   HandleEscape
.. code-block:: modula2
    PROCEDURE HandleEscape (s: String) : String ;
    
    
    END FormatStrings.

@c @node gm2-libs/FpuIO, gm2-libs/GetOpt, gm2-libs/FormatStrings, Base libraries
gm2-libs/FpuIO
--------------

.. code-block:: modula2
    DEFINITION MODULE FpuIO ;

    EXPORT QUALIFIED ReadReal, WriteReal, StrToReal, RealToStr,
                     ReadLongReal, WriteLongReal, StrToLongReal,
                     LongRealToStr,
                     ReadLongInt, WriteLongInt, StrToLongInt,
                     LongIntToStr ;
    
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (VAR x: REAL) ;
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (x: REAL; TotalWidth, FractionWidth: CARDINAL) ;
.. index::
   StrToReal
.. code-block:: modula2
    PROCEDURE StrToReal (a: ARRAY OF CHAR ; VAR x: REAL) ;
.. index::
   RealToStr
.. code-block:: modula2
    PROCEDURE RealToStr (x: REAL; TotalWidth, FractionWidth: CARDINAL;
                         VAR a: ARRAY OF CHAR) ;
    
.. index::
   ReadLongReal
.. code-block:: modula2
    PROCEDURE ReadLongReal (VAR x: LONGREAL) ;
.. index::
   WriteLongReal
.. code-block:: modula2
    PROCEDURE WriteLongReal (x: LONGREAL;
                             TotalWidth, FractionWidth: CARDINAL) ;
.. index::
   StrToLongReal
.. code-block:: modula2
    PROCEDURE StrToLongReal (a: ARRAY OF CHAR ; VAR x: LONGREAL) ;
.. index::
   LongRealToStr
.. code-block:: modula2
    PROCEDURE LongRealToStr (x: LONGREAL;
                             TotalWidth, FractionWidth: CARDINAL;
                             VAR a: ARRAY OF CHAR) ;
    
.. index::
   ReadLongInt
.. code-block:: modula2
    PROCEDURE ReadLongInt (VAR x: LONGINT) ;
.. index::
   WriteLongInt
.. code-block:: modula2
    PROCEDURE WriteLongInt (x: LONGINT; n: CARDINAL) ;
.. index::
   StrToLongInt
.. code-block:: modula2
    PROCEDURE StrToLongInt (a: ARRAY OF CHAR ; VAR x: LONGINT) ;
.. index::
   LongIntToStr
.. code-block:: modula2
    PROCEDURE LongIntToStr (x: LONGINT; n: CARDINAL; VAR a: ARRAY OF CHAR) ;
    
    
    END FpuIO.

@c @node gm2-libs/GetOpt, gm2-libs/IO, gm2-libs/FpuIO, Base libraries
gm2-libs/GetOpt
---------------

.. code-block:: modula2
    DEFINITION MODULE GetOpt ;

    FROM SYSTEM IMPORT ADDRESS ;
    FROM DynamicStrings IMPORT String ;
    
    CONST
.. index::
   pair: no_argument; (const)
.. code-block:: modula2
       no_argument = 0 ;
.. index::
   pair: required_argument; (const)
.. code-block:: modula2
       required_argument = 1 ;
.. index::
   pair: optional_argument; (const)
.. code-block:: modula2
       optional_argument = 2 ;
    
    TYPE
.. index::
   pair: LongOptions; (type)
.. code-block:: modula2
       LongOptions ;
.. index::
   pair: PtrToInteger; (type)
.. code-block:: modula2
       PtrToInteger = POINTER TO INTEGER ;
    
    (*
       GetOpt - call C getopt and fill in the parameters:
                optarg, optind, opterr and optop.
    *)
    
.. index::
   GetOpt
.. code-block:: modula2
    PROCEDURE GetOpt (argc: INTEGER; argv: ADDRESS; optstring: String;
                      VAR optarg: String;
                      VAR optind, opterr, optopt: INTEGER) : CHAR ;
    
    
    (*
       InitLongOptions - creates and returns a LongOptions empty array.
    *)
    
.. index::
   InitLongOptions
.. code-block:: modula2
    PROCEDURE InitLongOptions () : LongOptions ;
    
    
    (*
       AddLongOption - appends long option {name, has_arg, flag, val} to the
                       array of options and new long options array is
                       returned.
                       The old array, lo, should no longer be used.
    
       (from man 3 getopt)
           The meanings of the different fields are:
    
           name   is the name of the long option.
    
           has_arg
                  is: no_argument (or 0) if the option does not take an
                  argument; required_argument (or  1) if the option
                  requires an argument; or optional_argument (or 2) if
                  the option takes an optional argument.
    
           flag   specifies how results are returned for a long option.
                  If flag is NULL, then getopt_long() returns val.
                  (For example, the calling program may set val to the
                  equivalent short option character).  Otherwise,
                  getopt_long() returns 0, and flag points to a
                  variable which is set to val if the option is found,
                  but left unchanged if the option is not found.
    
           val    is the value to return, or to load into the variable
                  pointed to by flag.
    
           The last element of the array has to be filled with zeros.
    *)
    
.. index::
   AddLongOption
.. code-block:: modula2
    PROCEDURE AddLongOption (lo: LongOptions;
                             name: String; has_arg: INTEGER;
                             flag: PtrToInteger;
                             val: INTEGER) : LongOptions ;
    
    
    (*
       KillLongOptions - returns NIL and also frees up memory
                         associated with, lo.
    *)
    
.. index::
   KillLongOptions
.. code-block:: modula2
    PROCEDURE KillLongOptions (lo: LongOptions) : LongOptions ;
    
    
    (*
       GetOptLong - works like GetOpt but will accept long options (using
                    two dashes).  If the program only accepts long options
                    then optstring should be an empty string, not NIL.
    *)
    
.. index::
   GetOptLong
.. code-block:: modula2
    PROCEDURE GetOptLong (argc: INTEGER; argv: ADDRESS; optstring: String;
                          longopts: LongOptions;
                          VAR longindex: INTEGER) : INTEGER ;
    
    
    (*
       GetOptLongOnly - works like GetOptLong except that a single dash
                        can be used for a long option.
    *)
    
.. index::
   GetOptLongOnly
.. code-block:: modula2
    PROCEDURE GetOptLongOnly (argc: INTEGER; argv: ADDRESS;
                              optstring: String; longopts: LongOptions;
                              VAR longindex: INTEGER) : INTEGER ;
    
    
    END GetOpt.

@c @node gm2-libs/IO, gm2-libs/Indexing, gm2-libs/GetOpt, Base libraries
gm2-libs/IO
-----------

.. code-block:: modula2
    DEFINITION MODULE IO ;

(*
   Description: provides Read, Write, Errors procedures that map onto UNIX
                file descriptors 0, 1 and 2. This is achieved by using
                FIO if we are in buffered mode and using libc.write
                if not.
*)
    
    EXPORT QUALIFIED Read, Write, Error,
                     UnBufferedMode, BufferedMode,
                     EchoOn, EchoOff ;
    
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR ch: CHAR) ;
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (ch: CHAR) ;
.. index::
   Error
.. code-block:: modula2
    PROCEDURE Error (ch: CHAR) ;
    
    
    (*
       UnBufferedMode - places file descriptor, fd, into an unbuffered mode.
    *)
    
.. index::
   UnBufferedMode
.. code-block:: modula2
    PROCEDURE UnBufferedMode (fd: INTEGER; input: BOOLEAN) ;
    
    
    (*
       BufferedMode - places file descriptor, fd, into a buffered mode.
    *)
    
.. index::
   BufferedMode
.. code-block:: modula2
    PROCEDURE BufferedMode (fd: INTEGER; input: BOOLEAN) ;
    
    
    (*
       EchoOn - turns on echoing for file descriptor, fd.  This
                only really makes sence for a file descriptor opened
                for terminal input or maybe some specific file descriptor
                which is attached to a particular piece of hardware.
    *)
    
.. index::
   EchoOn
.. code-block:: modula2
    PROCEDURE EchoOn (fd: INTEGER; input: BOOLEAN) ;
    
    
    (*
       EchoOff - turns off echoing for file descriptor, fd.  This
                 only really makes sence for a file descriptor opened
                 for terminal input or maybe some specific file descriptor
                 which is attached to a particular piece of hardware.
    *)
    
.. index::
   EchoOff
.. code-block:: modula2
    PROCEDURE EchoOff (fd: INTEGER; input: BOOLEAN) ;
    
    
    END IO.

@c @node gm2-libs/Indexing, gm2-libs/LMathLib0, gm2-libs/IO, Base libraries
gm2-libs/Indexing
-----------------

.. code-block:: modula2
    DEFINITION MODULE Indexing ;

    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT QUALIFIED Index, InitIndex, KillIndex, GetIndice, PutIndice,
                     HighIndice, LowIndice, InBounds, IsIndiceInIndex,
                     RemoveIndiceFromIndex, IncludeIndiceIntoIndex,
                     ForeachIndiceInIndexDo, DeleteIndice, DebugIndex ;
    
    TYPE
.. index::
   pair: Index; (type)
.. code-block:: modula2
       Index ;
.. index::
   pair: IndexProcedure; (type)
.. code-block:: modula2
       IndexProcedure = PROCEDURE (ADDRESS) ;
    
    
    (*
       InitIndex - creates and returns an Index.
    *)
    
.. index::
   InitIndex
.. code-block:: modula2
    PROCEDURE InitIndex (low: CARDINAL) : Index ;
    
    
    (*
       KillIndex - returns Index to free storage.
    *)
    
.. index::
   KillIndex
.. code-block:: modula2
    PROCEDURE KillIndex (i: Index) : Index ;
    
    
    (*
       DebugIndex - turns on debugging within an index.
    *)
    
.. index::
   DebugIndex
.. code-block:: modula2
    PROCEDURE DebugIndex (i: Index) : Index ;
    
    
    (*
       InBounds - returns TRUE if indice, n, is within the bounds
                  of the dynamic array.
    *)
    
.. index::
   InBounds
.. code-block:: modula2
    PROCEDURE InBounds (i: Index; n: CARDINAL) : BOOLEAN ;
    
    
    (*
       HighIndice - returns the last legally accessible indice of this array.
    *)
    
.. index::
   HighIndice
.. code-block:: modula2
    PROCEDURE HighIndice (i: Index) : CARDINAL ;
    
    
    (*
       LowIndice - returns the first legally accessible indice of this array.
    *)
    
.. index::
   LowIndice
.. code-block:: modula2
    PROCEDURE LowIndice (i: Index) : CARDINAL ;
    
    
    (*
       PutIndice - places, a, into the dynamic array at position i[n]
    *)
    
.. index::
   PutIndice
.. code-block:: modula2
    PROCEDURE PutIndice (i: Index; n: CARDINAL; a: ADDRESS) ;
    
    
    (*
       GetIndice - retrieves, element i[n] from the dynamic array.
    *)
    
.. index::
   GetIndice
.. code-block:: modula2
    PROCEDURE GetIndice (i: Index; n: CARDINAL) : ADDRESS ;
    
    
    (*
       IsIndiceInIndex - returns TRUE if, a, is in the index, i.
    *)
    
.. index::
   IsIndiceInIndex
.. code-block:: modula2
    PROCEDURE IsIndiceInIndex (i: Index; a: ADDRESS) : BOOLEAN ;
    
    
    (*
       RemoveIndiceFromIndex - removes, a, from Index, i.
    *)
    
.. index::
   RemoveIndiceFromIndex
.. code-block:: modula2
    PROCEDURE RemoveIndiceFromIndex (i: Index; a: ADDRESS) ;
    
    
    (*
       DeleteIndice - delete i[j] from the array.
    *)
    
.. index::
   DeleteIndice
.. code-block:: modula2
    PROCEDURE DeleteIndice (i: Index; j: CARDINAL) ;
    
    
    (*
       IncludeIndiceIntoIndex - if the indice is not in the index, then
                                add it at the end.
    *)
    
.. index::
   IncludeIndiceIntoIndex
.. code-block:: modula2
    PROCEDURE IncludeIndiceIntoIndex (i: Index; a: ADDRESS) ;
    
    
    (*
       ForeachIndiceInIndexDo - for each j indice of i, call procedure p(i[j])
    *)
    
.. index::
   ForeachIndiceInIndexDo
.. code-block:: modula2
    PROCEDURE ForeachIndiceInIndexDo (i: Index; p: IndexProcedure) ;
    
    
    END Indexing.

@c @node gm2-libs/LMathLib0, gm2-libs/LegacyReal, gm2-libs/Indexing, Base libraries
gm2-libs/LMathLib0
------------------

.. code-block:: modula2
    DEFINITION MODULE LMathLib0 ;

    CONST
       pi   = 3.1415926535897932384626433832795028841972;
       exp1 = 2.7182818284590452353602874713526624977572;
    
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: LONGREAL) : LONGREAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (x: LONGREAL) : LONGREAL ;
.. index::
   ln
.. code-block:: modula2
    PROCEDURE ln (x: LONGREAL) : LONGREAL ;
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: LONGREAL) : LONGREAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: LONGREAL) : LONGREAL ;
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (x: LONGREAL) : LONGREAL ;
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (x: LONGREAL) : LONGREAL ;
.. index::
   entier
.. code-block:: modula2
    PROCEDURE entier (x: LONGREAL) : INTEGER ;
    
    
    END LMathLib0.

@c @node gm2-libs/LegacyReal, gm2-libs/M2Dependent, gm2-libs/LMathLib0, Base libraries
gm2-libs/LegacyReal
-------------------

.. code-block:: modula2
    DEFINITION MODULE LegacyReal ;

    TYPE
       REAL = SHORTREAL ;
    
    
    END LegacyReal.

@c @node gm2-libs/M2Dependent, gm2-libs/M2EXCEPTION, gm2-libs/LegacyReal, Base libraries
gm2-libs/M2Dependent
--------------------

.. code-block:: modula2
    DEFINITION MODULE M2Dependent ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    
    TYPE
.. index::
   pair: ArgCVEnvP; (type)
.. code-block:: modula2
       ArgCVEnvP = PROCEDURE (INTEGER, ADDRESS, ADDRESS) ;
    
    
.. index::
   ConstructModules
.. code-block:: modula2
    PROCEDURE ConstructModules (applicationmodule: ADDRESS;
                                argc: INTEGER; argv, envp: ADDRESS) ;
    
.. index::
   DeconstructModules
.. code-block:: modula2
    PROCEDURE DeconstructModules (applicationmodule: ADDRESS;
                                  argc: INTEGER; argv, envp: ADDRESS) ;
    
    
    (*
       RegisterModule - adds module name to the list of outstanding
                        modules which need to have their dependencies
                        explored to determine initialization order.
    *)
    
.. index::
   RegisterModule
.. code-block:: modula2
    PROCEDURE RegisterModule (name: ADDRESS;
                              init, fini:  ArgCVEnvP;
                              dependencies: PROC) ;
    
    
    (*
       RequestDependant - used to specify that modulename is dependant upon
                          module dependantmodule.
    *)
    
.. index::
   RequestDependant
.. code-block:: modula2
    PROCEDURE RequestDependant (modulename, dependantmodule: ADDRESS) ;
    
    
    END M2Dependent.

@c @node gm2-libs/M2EXCEPTION, gm2-libs/M2LINK, gm2-libs/M2Dependent, Base libraries
gm2-libs/M2EXCEPTION
--------------------

.. code-block:: modula2
    DEFINITION MODULE M2EXCEPTION;

    
    (* This enumerated list of exceptions must match the exceptions in gm2-libs-iso to
       allow mixed module dialect projects.  *)
    
    TYPE
.. index::
   pair: M2Exceptions; (type)
.. code-block:: modula2
      M2Exceptions =
        (indexException,     rangeException,         caseSelectException,  invalidLocation,
         functionException,  wholeValueException,    wholeDivException,    realValueException,
         realDivException,   complexValueException,  complexDivException,  protException,
         sysException,       coException,            exException
        );
    
    
    (* If the program or coroutine is in the exception state then return the enumeration
       value representing the exception cause.  If it is not in the exception state then
       raises and exception (exException).  *)
    
.. index::
   M2Exception
.. code-block:: modula2
    PROCEDURE M2Exception () : M2Exceptions;
    
    (* Returns TRUE if the program or coroutine is in the exception state.
       Returns FALSE if the program or coroutine is not in the exception state.  *)
    
.. index::
   IsM2Exception
.. code-block:: modula2
    PROCEDURE IsM2Exception () : BOOLEAN;
    
    
    END M2EXCEPTION.

@c @node gm2-libs/M2LINK, gm2-libs/M2RTS, gm2-libs/M2EXCEPTION, Base libraries
gm2-libs/M2LINK
---------------

.. code-block:: modula2
    DEFINITION MODULE FOR "C" M2LINK ;

    
    TYPE
.. index::
   pair: PtrToChar; (type)
.. code-block:: modula2
       PtrToChar = POINTER TO CHAR ;
    
    (* These variables are set by the compiler in the program module
       according to linking command line options.  *)
    
    VAR
.. index::
   pair: ForcedModuleInitOrder; (var)
.. code-block:: modula2
       ForcedModuleInitOrder: PtrToChar ;
.. index::
   pair: StaticInitialization; (var)
.. code-block:: modula2
       StaticInitialization : BOOLEAN ;
    
    
.. index::
   pair: END M2LINK.; (var)
.. code-block:: modula2
    END M2LINK.

@c @node gm2-libs/M2RTS, gm2-libs/MathLib0, gm2-libs/M2LINK, Base libraries
gm2-libs/M2RTS
--------------

.. code-block:: modula2
    DEFINITION MODULE M2RTS ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    
    TYPE
.. index::
   pair: ArgCVEnvP; (type)
.. code-block:: modula2
       ArgCVEnvP = PROCEDURE (INTEGER, ADDRESS, ADDRESS) ;
    
    
.. index::
   ConstructModules
.. code-block:: modula2
    PROCEDURE ConstructModules (applicationmodule: ADDRESS;
                                argc: INTEGER; argv, envp: ADDRESS) ;
    
.. index::
   DeconstructModules
.. code-block:: modula2
    PROCEDURE DeconstructModules (applicationmodule: ADDRESS;
                                  argc: INTEGER; argv, envp: ADDRESS) ;
    
    
    (*
       RegisterModule - adds module name to the list of outstanding
                        modules which need to have their dependencies
                        explored to determine initialization order.
    *)
    
.. index::
   RegisterModule
.. code-block:: modula2
    PROCEDURE RegisterModule (name: ADDRESS;
                              init, fini:  ArgCVEnvP;
                              dependencies: PROC) ;
    
    
    (*
       RequestDependant - used to specify that modulename is dependant upon
                          module dependantmodule.
    *)
    
.. index::
   RequestDependant
.. code-block:: modula2
    PROCEDURE RequestDependant (modulename, dependantmodule: ADDRESS) ;
    
    
    (*
       InstallTerminationProcedure - installs a procedure, p, which will
                                     be called when the procedure
                                     ExecuteTerminationProcedures
                                     is invoked.  It returns TRUE is the
                                     procedure is installed.
    *)
    
.. index::
   InstallTerminationProcedure
.. code-block:: modula2
    PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;
    
    
    (*
       ExecuteInitialProcedures - executes the initial procedures installed
                                  by InstallInitialProcedure.
    *)
    
.. index::
   ExecuteInitialProcedures
.. code-block:: modula2
    PROCEDURE ExecuteInitialProcedures ;
    
    
    (*
       InstallInitialProcedure - installs a procedure to be executed just
                                 before the BEGIN code section of the main
                                 program module.
    *)
    
.. index::
   InstallInitialProcedure
.. code-block:: modula2
    PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;
    
    
    (*
       ExecuteTerminationProcedures - calls each installed termination procedure
                                      in reverse order.
    *)
    
.. index::
   ExecuteTerminationProcedures
.. code-block:: modula2
    PROCEDURE ExecuteTerminationProcedures ;
    
    
    (*
       Terminate - provides compatibility for pim.  It call exit with
                   the exitcode provided in a prior call to ExitOnHalt
                   (or zero if ExitOnHalt was never called).  It does
                   not call ExecuteTerminationProcedures.
    *)
    
.. index::
   Terminate
.. code-block:: modula2
    PROCEDURE Terminate <* noreturn *> ;
    
    
    (*
       HALT - terminate the current program.  The procedure Terminate
              is called before the program is stopped.  The parameter
              exitcode is optional.  If the parameter is not supplied
              HALT will call libc 'abort', otherwise it will exit with
              the code supplied.  Supplying a parameter to HALT has the
              same effect as calling ExitOnHalt with the same code and
              then calling HALT with no parameter.
    *)
    
.. index::
   HALT
.. code-block:: modula2
    PROCEDURE HALT ([exitcode: INTEGER = -1]) <* noreturn *> ;
    
    
    (*
       Halt - provides a more user friendly version of HALT, which takes
              four parameters to aid debugging.
    *)
    
.. index::
   Halt
.. code-block:: modula2
    PROCEDURE Halt (file: ARRAY OF CHAR; line: CARDINAL;
                    function: ARRAY OF CHAR; description: ARRAY OF CHAR)
    		<* noreturn *> ;
    
    
    (*
       ExitOnHalt - if HALT is executed then call exit with the exit code, e.
    *)
    
.. index::
   ExitOnHalt
.. code-block:: modula2
    PROCEDURE ExitOnHalt (e: INTEGER) ;
    
    
    (*
       ErrorMessage - emits an error message to stderr and then calls exit (1).
    *)
    
.. index::
   ErrorMessage
.. code-block:: modula2
    PROCEDURE ErrorMessage (message: ARRAY OF CHAR;
                            file: ARRAY OF CHAR;
                            line: CARDINAL;
                            function: ARRAY OF CHAR) <* noreturn *> ;
    
    
    (*
       Length - returns the length of a string, a. This is called whenever
                the user calls LENGTH and the parameter cannot be calculated
                at compile time.
    *)
    
.. index::
   Length
.. code-block:: modula2
    PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
    
    
    (*
       The following are the runtime exception handler routines.
    *)
    
.. index::
   AssignmentException
.. code-block:: modula2
    PROCEDURE AssignmentException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ReturnException
.. code-block:: modula2
    PROCEDURE ReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   IncException
.. code-block:: modula2
    PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   DecException
.. code-block:: modula2
    PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   InclException
.. code-block:: modula2
    PROCEDURE InclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ExclException
.. code-block:: modula2
    PROCEDURE ExclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ShiftException
.. code-block:: modula2
    PROCEDURE ShiftException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   RotateException
.. code-block:: modula2
    PROCEDURE RotateException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   StaticArraySubscriptException
.. code-block:: modula2
    PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   DynamicArraySubscriptException
.. code-block:: modula2
    PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ForLoopBeginException
.. code-block:: modula2
    PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ForLoopToException
.. code-block:: modula2
    PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ForLoopEndException
.. code-block:: modula2
    PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   PointerNilException
.. code-block:: modula2
    PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   NoReturnException
.. code-block:: modula2
    PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   CaseException
.. code-block:: modula2
    PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeNonPosDivException
.. code-block:: modula2
    PROCEDURE WholeNonPosDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeNonPosModException
.. code-block:: modula2
    PROCEDURE WholeNonPosModException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeZeroDivException
.. code-block:: modula2
    PROCEDURE WholeZeroDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeZeroRemException
.. code-block:: modula2
    PROCEDURE WholeZeroRemException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeValueException
.. code-block:: modula2
    PROCEDURE WholeValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   RealValueException
.. code-block:: modula2
    PROCEDURE RealValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ParameterException
.. code-block:: modula2
    PROCEDURE ParameterException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   NoException
.. code-block:: modula2
    PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
    
    
    END M2RTS.

@c @node gm2-libs/MathLib0, gm2-libs/MemUtils, gm2-libs/M2RTS, Base libraries
gm2-libs/MathLib0
-----------------

.. code-block:: modula2
    DEFINITION MODULE MathLib0 ;

    CONST
       pi   = 3.1415926535897932384626433832795028841972;
       exp1 = 2.7182818284590452353602874713526624977572;
    
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: REAL) : REAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (x: REAL) : REAL ;
.. index::
   ln
.. code-block:: modula2
    PROCEDURE ln (x: REAL) : REAL ;
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: REAL) : REAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: REAL) : REAL ;
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (x: REAL) : REAL ;
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (x: REAL) : REAL ;
.. index::
   entier
.. code-block:: modula2
    PROCEDURE entier (x: REAL) : INTEGER ;
    
    
    END MathLib0.

@c @node gm2-libs/MemUtils, gm2-libs/NumberIO, gm2-libs/MathLib0, Base libraries
gm2-libs/MemUtils
-----------------

.. code-block:: modula2
    DEFINITION MODULE MemUtils ;

    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT QUALIFIED MemCopy, MemZero ;
    
    
    (*
       MemCopy - copys a region of memory to the required destination.
    *)
    
.. index::
   MemCopy
.. code-block:: modula2
    PROCEDURE MemCopy (from: ADDRESS; length: CARDINAL; to: ADDRESS) ;
    
    
    (*
       MemZero - sets a region of memory: a..a+length to zero.
    *)
    
.. index::
   MemZero
.. code-block:: modula2
    PROCEDURE MemZero (a: ADDRESS; length: CARDINAL) ;
    
    
    END MemUtils.

@c @node gm2-libs/NumberIO, gm2-libs/OptLib, gm2-libs/MemUtils, Base libraries
gm2-libs/NumberIO
-----------------

.. code-block:: modula2
    DEFINITION MODULE NumberIO ;

    EXPORT QUALIFIED ReadCard, WriteCard, ReadHex, WriteHex, ReadInt, WriteInt,
                     CardToStr, StrToCard, StrToHex, HexToStr, StrToInt, IntToStr,
                     ReadOct, WriteOct, OctToStr, StrToOct,
                     ReadBin, WriteBin, BinToStr, StrToBin,
                     StrToBinInt, StrToHexInt, StrToOctInt ;
    
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (VAR x: CARDINAL) ;
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (x, n: CARDINAL) ;
    
.. index::
   ReadHex
.. code-block:: modula2
    PROCEDURE ReadHex (VAR x: CARDINAL) ;
    
.. index::
   WriteHex
.. code-block:: modula2
    PROCEDURE WriteHex (x, n: CARDINAL) ;
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (VAR x: INTEGER) ;
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (x: INTEGER ; n: CARDINAL) ;
    
.. index::
   CardToStr
.. code-block:: modula2
    PROCEDURE CardToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
    
.. index::
   StrToCard
.. code-block:: modula2
    PROCEDURE StrToCard (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
    
.. index::
   HexToStr
.. code-block:: modula2
    PROCEDURE HexToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
    
.. index::
   StrToHex
.. code-block:: modula2
    PROCEDURE StrToHex (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
    
.. index::
   IntToStr
.. code-block:: modula2
    PROCEDURE IntToStr (x: INTEGER ; n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
    
.. index::
   StrToInt
.. code-block:: modula2
    PROCEDURE StrToInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
    
.. index::
   ReadOct
.. code-block:: modula2
    PROCEDURE ReadOct (VAR x: CARDINAL) ;
    
.. index::
   WriteOct
.. code-block:: modula2
    PROCEDURE WriteOct (x, n: CARDINAL) ;
    
.. index::
   OctToStr
.. code-block:: modula2
    PROCEDURE OctToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
    
.. index::
   StrToOct
.. code-block:: modula2
    PROCEDURE StrToOct (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
    
.. index::
   ReadBin
.. code-block:: modula2
    PROCEDURE ReadBin (VAR x: CARDINAL) ;
    
.. index::
   WriteBin
.. code-block:: modula2
    PROCEDURE WriteBin (x, n: CARDINAL) ;
    
.. index::
   BinToStr
.. code-block:: modula2
    PROCEDURE BinToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
    
.. index::
   StrToBin
.. code-block:: modula2
    PROCEDURE StrToBin (a: ARRAY OF CHAR ; VAR x: CARDINAL) ;
    
.. index::
   StrToBinInt
.. code-block:: modula2
    PROCEDURE StrToBinInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
    
.. index::
   StrToHexInt
.. code-block:: modula2
    PROCEDURE StrToHexInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
    
.. index::
   StrToOctInt
.. code-block:: modula2
    PROCEDURE StrToOctInt (a: ARRAY OF CHAR ; VAR x: INTEGER) ;
    
    
    END NumberIO.

@c @node gm2-libs/OptLib, gm2-libs/PushBackInput, gm2-libs/NumberIO, Base libraries
gm2-libs/OptLib
---------------

.. code-block:: modula2
    DEFINITION MODULE OptLib ;

    FROM SYSTEM IMPORT ADDRESS ;
    FROM DynamicStrings IMPORT String ;
    
    TYPE
.. index::
   pair: Option; (type)
.. code-block:: modula2
       Option ;
    
    
    (*
       InitOption - constructor for Option.
    *)
    
.. index::
   InitOption
.. code-block:: modula2
    PROCEDURE InitOption (argc: INTEGER; argv: ADDRESS) : Option ;
    
    
    (*
       KillOption - deconstructor for Option.
    *)
    
.. index::
   KillOption
.. code-block:: modula2
    PROCEDURE KillOption (o: Option) : Option ;
    
    
    (*
       Dup - duplicate the option array inside, o.
             Notice that this does not duplicate all the contents
             (strings) of argv.
             Shallow copy of the top level indices.
    *)
    
.. index::
   Dup
.. code-block:: modula2
    PROCEDURE Dup (o: Option) : Option ;
    
    
    (*
       Slice - return a new option which has elements [low:high] from the
               options, o.
    *)
    
.. index::
   Slice
.. code-block:: modula2
    PROCEDURE Slice (o: Option; low, high: INTEGER) : Option ;
    
    
    (*
       IndexStrCmp - returns the index in the argv array which matches
                     string, s.  -1 is returned if the string is not found.
    *)
    
.. index::
   IndexStrCmp
.. code-block:: modula2
    PROCEDURE IndexStrCmp (o: Option; s: String) : INTEGER ;
    
    
    (*
       IndexStrNCmp - returns the index in the argv array where the first
                      characters are matched by string, s.
                      -1 is returned if the string is not found.
    *)
    
.. index::
   IndexStrNCmp
.. code-block:: modula2
    PROCEDURE IndexStrNCmp (o: Option; s: String) : INTEGER ;
    
    
    (*
       ConCat - returns the concatenation of a and b.
    *)
    
.. index::
   ConCat
.. code-block:: modula2
    PROCEDURE ConCat (a, b: Option) : Option ;
    
    
    (*
       GetArgv - return the argv component of option.
    *)
    
.. index::
   GetArgv
.. code-block:: modula2
    PROCEDURE GetArgv (o: Option) : ADDRESS ;
    
    
    (*
       GetArgc - return the argc component of option.
    *)
    
.. index::
   GetArgc
.. code-block:: modula2
    PROCEDURE GetArgc (o: Option) : INTEGER ;
    
    
    END OptLib.

@c @node gm2-libs/PushBackInput, gm2-libs/RTExceptions, gm2-libs/OptLib, Base libraries
gm2-libs/PushBackInput
----------------------

.. code-block:: modula2
    DEFINITION MODULE PushBackInput ;

    FROM FIO IMPORT File ;
    FROM DynamicStrings IMPORT String ;
    
    EXPORT QUALIFIED Open, PutCh, GetCh, Error, WarnError, WarnString,
                     Close, SetDebug, GetExitStatus, PutStr,
                     PutString, GetColumnPosition, GetCurrentLine ;
    
    
    (*
       Open - opens a file for reading.
    *)
    
.. index::
   Open
.. code-block:: modula2
    PROCEDURE Open (a: ARRAY OF CHAR) : File ;
    
    
    (*
       GetCh - gets a character from either the push back stack or
               from file, f.
    *)
    
.. index::
   GetCh
.. code-block:: modula2
    PROCEDURE GetCh (f: File) : CHAR ;
    
    
    (*
       PutCh - pushes a character onto the push back stack, it also
               returns the character which has been pushed.
    *)
    
.. index::
   PutCh
.. code-block:: modula2
    PROCEDURE PutCh (ch: CHAR) : CHAR ;
    
    
    (*
       PutString - pushes a string onto the push back stack.
    *)
    
.. index::
   PutString
.. code-block:: modula2
    PROCEDURE PutString (a: ARRAY OF CHAR) ;
    
    
    (*
       PutStr - pushes a dynamic string onto the push back stack.
                The string, s, is not deallocated.
    *)
    
.. index::
   PutStr
.. code-block:: modula2
    PROCEDURE PutStr (s: String) ;
    
    
    (*
       Error - emits an error message with the appropriate file, line combination.
    *)
    
.. index::
   Error
.. code-block:: modula2
    PROCEDURE Error (a: ARRAY OF CHAR) ;
    
    
    (*
       WarnError - emits an error message with the appropriate file, line combination.
                   It does not terminate but when the program finishes an exit status of
                   1 will be issued.
    *)
    
.. index::
   WarnError
.. code-block:: modula2
    PROCEDURE WarnError (a: ARRAY OF CHAR) ;
    
    
    (*
       WarnString - emits an error message with the appropriate file, line combination.
                    It does not terminate but when the program finishes an exit status of
                    1 will be issued.
    *)
    
.. index::
   WarnString
.. code-block:: modula2
    PROCEDURE WarnString (s: String) ;
    
    
    (*
       Close - closes the opened file.
    *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (f: File) ;
    
    
    (*
       GetExitStatus - returns the exit status which will be 1 if any warnings were issued.
    *)
    
.. index::
   GetExitStatus
.. code-block:: modula2
    PROCEDURE GetExitStatus () : CARDINAL ;
    
    
    (*
       SetDebug - sets the debug flag on or off.
    *)
    
.. index::
   SetDebug
.. code-block:: modula2
    PROCEDURE SetDebug (d: BOOLEAN) ;
    
    
    (*
       GetColumnPosition - returns the column position of the current character.
    *)
    
.. index::
   GetColumnPosition
.. code-block:: modula2
    PROCEDURE GetColumnPosition () : CARDINAL ;
    
    
    (*
       GetCurrentLine - returns the current line number.
    *)
    
.. index::
   GetCurrentLine
.. code-block:: modula2
    PROCEDURE GetCurrentLine () : CARDINAL ;
    
    
    END PushBackInput.

@c @node gm2-libs/RTExceptions, gm2-libs/RTint, gm2-libs/PushBackInput, Base libraries
gm2-libs/RTExceptions
---------------------

.. code-block:: modula2
    DEFINITION MODULE RTExceptions ;

(* Runtime exception handler routines.  This should
   be considered as a system module for GNU Modula-2
   and allow the compiler to interface with exception
   handling.  *)
    
    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT QUALIFIED EHBlock,
                     Raise, SetExceptionBlock, GetExceptionBlock,
                     GetTextBuffer, GetTextBufferSize, GetNumber,
                     InitExceptionBlock, KillExceptionBlock,
                     PushHandler, PopHandler,
                     BaseExceptionsThrow, DefaultErrorCatch,
                     IsInExceptionState, SetExceptionState,
                     SwitchExceptionState, GetBaseExceptionBlock,
                     SetExceptionSource, GetExceptionSource ;
    
    TYPE
.. index::
   pair: EHBlock; (type)
.. code-block:: modula2
       EHBlock ;
.. index::
   pair: ProcedureHandler; (type)
.. code-block:: modula2
       ProcedureHandler = PROCEDURE ;
    
    
    (*
       Raise - invoke the exception handler associated with, number,
               in the active EHBlock.  It keeps a record of the number
               and message in the EHBlock for later use.
    *)
    
.. index::
   Raise
.. code-block:: modula2
    PROCEDURE Raise (number: CARDINAL;
                     file: ADDRESS; line: CARDINAL;
                     column: CARDINAL; function: ADDRESS;
                     message: ADDRESS) ;
    
    
    (*
       SetExceptionBlock - sets, source, as the active EHB.
    *)
    
.. index::
   SetExceptionBlock
.. code-block:: modula2
    PROCEDURE SetExceptionBlock (source: EHBlock) ;
    
    
    (*
       GetExceptionBlock - returns the active EHB.
    *)
    
.. index::
   GetExceptionBlock
.. code-block:: modula2
    PROCEDURE GetExceptionBlock () : EHBlock ;
    
    
    (*
       GetTextBuffer - returns the address of the EHB buffer.
    *)
    
.. index::
   GetTextBuffer
.. code-block:: modula2
    PROCEDURE GetTextBuffer (e: EHBlock) : ADDRESS ;
    
    
    (*
       GetTextBufferSize - return the size of the EHB text buffer.
    *)
    
.. index::
   GetTextBufferSize
.. code-block:: modula2
    PROCEDURE GetTextBufferSize (e: EHBlock) : CARDINAL ;
    
    
    (*
       GetNumber - return the exception number associated with,
                   source.
    *)
    
.. index::
   GetNumber
.. code-block:: modula2
    PROCEDURE GetNumber (source: EHBlock) : CARDINAL ;
    
    
    (*
       InitExceptionBlock - creates and returns a new exception block.
    *)
    
.. index::
   InitExceptionBlock
.. code-block:: modula2
    PROCEDURE InitExceptionBlock () : EHBlock ;
    
    
    (*
       KillExceptionBlock - destroys the EHB, e, and all its handlers.
    *)
    
.. index::
   KillExceptionBlock
.. code-block:: modula2
    PROCEDURE KillExceptionBlock (e: EHBlock) : EHBlock ;
    
    
    (*
       PushHandler - install a handler in EHB, e.
    *)
    
.. index::
   PushHandler
.. code-block:: modula2
    PROCEDURE PushHandler (e: EHBlock; number: CARDINAL; p: ProcedureHandler) ;
    
    
    (*
       PopHandler - removes the handler associated with, number, from
                    EHB, e.
    *)
    
.. index::
   PopHandler
.. code-block:: modula2
    PROCEDURE PopHandler (e: EHBlock; number: CARDINAL) ;
    
    
    (*
       DefaultErrorCatch - displays the current error message in
                           the current exception block and then
                           calls HALT.
    *)
    
.. index::
   DefaultErrorCatch
.. code-block:: modula2
    PROCEDURE DefaultErrorCatch ;
    
    
    (*
       BaseExceptionsThrow - configures the Modula-2 exceptions to call
                             THROW which in turn can be caught by an
                             exception block.  If this is not called then
                             a Modula-2 exception will simply call an
                             error message routine and then HALT.
    *)
    
.. index::
   BaseExceptionsThrow
.. code-block:: modula2
    PROCEDURE BaseExceptionsThrow ;
    
    
    (*
       IsInExceptionState - returns TRUE if the program is currently
                            in the exception state.
    *)
    
.. index::
   IsInExceptionState
.. code-block:: modula2
    PROCEDURE IsInExceptionState () : BOOLEAN ;
    
    
    (*
       SetExceptionState - returns the current exception state and
                           then sets the current exception state to,
                           to.
    *)
    
.. index::
   SetExceptionState
.. code-block:: modula2
    PROCEDURE SetExceptionState (to: BOOLEAN) : BOOLEAN ;
    
    
    (*
       SwitchExceptionState - assigns, from, with the current exception
                              state and then assigns the current exception
                              to, to.
    *)
    
.. index::
   SwitchExceptionState
.. code-block:: modula2
    PROCEDURE SwitchExceptionState (VAR from: BOOLEAN; to: BOOLEAN) ;
    
    
    (*
       GetBaseExceptionBlock - returns the initial language exception block
                               created.
    *)
    
.. index::
   GetBaseExceptionBlock
.. code-block:: modula2
    PROCEDURE GetBaseExceptionBlock () : EHBlock ;
    
    
    (*
       SetExceptionSource - sets the current exception source to, source.
    *)
    
.. index::
   SetExceptionSource
.. code-block:: modula2
    PROCEDURE SetExceptionSource (source: ADDRESS) ;
    
    
    (*
       GetExceptionSource - returns the current exception source.
    *)
    
.. index::
   GetExceptionSource
.. code-block:: modula2
    PROCEDURE GetExceptionSource () : ADDRESS ;
    
    
    END RTExceptions.

@c @node gm2-libs/RTint, gm2-libs/SArgs, gm2-libs/RTExceptions, Base libraries
gm2-libs/RTint
--------------

.. code-block:: modula2
    DEFINITION MODULE RTint ;

(* Provides users of the COROUTINES library with the
   ability to create interrupt sources based on
   file descriptors and timeouts.  *)
    
    FROM SYSTEM IMPORT ADDRESS ;
    
    TYPE
.. index::
   pair: DispatchVector; (type)
.. code-block:: modula2
       DispatchVector = PROCEDURE (CARDINAL, CARDINAL, ADDRESS) ;
    
    
    (*
       InitInputVector - returns an interrupt vector which is associated
                         with the file descriptor, fd.
    *)
    
.. index::
   InitInputVector
.. code-block:: modula2
    PROCEDURE InitInputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
    
    
    (*
       InitOutputVector - returns an interrupt vector which is associated
                          with the file descriptor, fd.
    *)
    
.. index::
   InitOutputVector
.. code-block:: modula2
    PROCEDURE InitOutputVector (fd: INTEGER; pri: CARDINAL) : CARDINAL ;
    
    
    (*
       InitTimeVector - returns an interrupt vector associated with
                        the relative time.
    *)
    
.. index::
   InitTimeVector
.. code-block:: modula2
    PROCEDURE InitTimeVector (micro, secs: CARDINAL; pri: CARDINAL) : CARDINAL ;
    
    
    (*
       ReArmTimeVector - reprimes the vector, vec, to deliver an interrupt
                         at the new relative time.
    *)
    
.. index::
   ReArmTimeVector
.. code-block:: modula2
    PROCEDURE ReArmTimeVector (vec: CARDINAL; micro, secs: CARDINAL) ;
    
    
    (*
       GetTimeVector - assigns, micro, and, secs, with the remaining
                       time before this interrupt will expire.
                       This value is only updated when a Listen
                       occurs.
    *)
    
.. index::
   GetTimeVector
.. code-block:: modula2
    PROCEDURE GetTimeVector (vec: CARDINAL; VAR micro, secs: CARDINAL) ;
    
    
    (*
       AttachVector - adds the pointer, p, to be associated with the interrupt
                      vector. It returns the previous value attached to this
                      vector.
    *)
    
.. index::
   AttachVector
.. code-block:: modula2
    PROCEDURE AttachVector (vec: CARDINAL; p: ADDRESS) : ADDRESS ;
    
    
    (*
       IncludeVector - includes, vec, into the dispatcher list of
                       possible interrupt causes.
    *)
    
.. index::
   IncludeVector
.. code-block:: modula2
    PROCEDURE IncludeVector (vec: CARDINAL) ;
    
    
    (*
       ExcludeVector - excludes, vec, from the dispatcher list of
                       possible interrupt causes.
    *)
    
.. index::
   ExcludeVector
.. code-block:: modula2
    PROCEDURE ExcludeVector (vec: CARDINAL) ;
    
    
    (*
       Listen - will either block indefinitely (until an interrupt)
                or alteratively will test to see whether any interrupts
                are pending.
                If a pending interrupt was found then, call, is called
                and then this procedure returns.
                It only listens for interrupts > pri.
    *)
    
.. index::
   Listen
.. code-block:: modula2
    PROCEDURE Listen (untilInterrupt: BOOLEAN;
                      call: DispatchVector;
                      pri: CARDINAL) ;
    
    
    (*
       Init - allows the user to force the initialize order.
    *)
    
.. index::
   Init
.. code-block:: modula2
    PROCEDURE Init ;
    
    
    END RTint.

@c @node gm2-libs/SArgs, gm2-libs/SCmdArgs, gm2-libs/RTint, Base libraries
gm2-libs/SArgs
--------------

.. code-block:: modula2
    DEFINITION MODULE SArgs ;

    FROM DynamicStrings IMPORT String ;
    EXPORT QUALIFIED GetArg, Narg ;
    
    
    (*
       GetArg - returns the nth argument from the command line.
                The success of the operation is returned.
                If TRUE is returned then the string, s, contains a
                new string, otherwise s is set to NIL.
    *)
    
.. index::
   GetArg
.. code-block:: modula2
    PROCEDURE GetArg (VAR s: String ; n: CARDINAL) : BOOLEAN ;
    
    
    (*
       Narg - returns the number of arguments available from
              command line.
    *)
    
.. index::
   Narg
.. code-block:: modula2
    PROCEDURE Narg() : CARDINAL ;
    
    
    END SArgs.

@c @node gm2-libs/SCmdArgs, gm2-libs/SEnvironment, gm2-libs/SArgs, Base libraries
gm2-libs/SCmdArgs
-----------------

.. code-block:: modula2
    DEFINITION MODULE SCmdArgs ;

    FROM DynamicStrings IMPORT String ;
    
    EXPORT QUALIFIED GetArg, Narg ;
    
    
    (*
       GetArg - returns the nth argument from the command line, CmdLine
                the success of the operation is returned.
    *)
    
.. index::
   GetArg
.. code-block:: modula2
    PROCEDURE GetArg (CmdLine: String;
                      n: CARDINAL; VAR Argi: String) : BOOLEAN ;
    
    
    (*
       Narg - returns the number of arguments available from
              command line, CmdLine.
    *)
    
.. index::
   Narg
.. code-block:: modula2
    PROCEDURE Narg (CmdLine: String) : CARDINAL ;
    
    
    END SCmdArgs.

@c @node gm2-libs/SEnvironment, gm2-libs/SFIO, gm2-libs/SCmdArgs, Base libraries
gm2-libs/SEnvironment
---------------------

.. code-block:: modula2
    DEFINITION MODULE SEnvironment ;

    
    FROM DynamicStrings IMPORT String ;
    EXPORT QUALIFIED GetEnvironment ;
    
    
    (*
       GetEnvironment - gets the environment variable Env and places
          	       	    a copy of its value into String, dest.
                        It returns TRUE if the string Env was found in
                        the processes environment.
    *)
    
.. index::
   GetEnvironment
.. code-block:: modula2
    PROCEDURE GetEnvironment (Env: String;
                              VAR dest: String) : BOOLEAN ;
    
    
    (*
       PutEnvironment - change or add an environment variable definition EnvDef.
                        TRUE is returned if the environment variable was
                        set or changed successfully.
    *)
    
.. index::
   PutEnvironment
.. code-block:: modula2
    PROCEDURE PutEnvironment (EnvDef: String) : BOOLEAN ;
    
    
    END SEnvironment.

@c @node gm2-libs/SFIO, gm2-libs/SMathLib0, gm2-libs/SEnvironment, Base libraries
gm2-libs/SFIO
-------------

.. code-block:: modula2
    DEFINITION MODULE SFIO ;

    FROM DynamicStrings IMPORT String ;
    FROM FIO IMPORT File ;
    
    EXPORT QUALIFIED OpenToRead, OpenToWrite, OpenForRandom, Exists, WriteS, ReadS ;
    
    
    (*
       Exists - returns TRUE if a file named, fname exists for reading.
    *)
    
.. index::
   Exists
.. code-block:: modula2
    PROCEDURE Exists (fname: String) : BOOLEAN ;
    
    
    (*
       OpenToRead - attempts to open a file, fname, for reading and
                    it returns this file.
                    The success of this operation can be checked by
                    calling IsNoError.
    *)
    
.. index::
   OpenToRead
.. code-block:: modula2
    PROCEDURE OpenToRead (fname: String) : File ;
    
    
    (*
       OpenToWrite - attempts to open a file, fname, for write and
                     it returns this file.
                     The success of this operation can be checked by
                     calling IsNoError.
    *)
    
.. index::
   OpenToWrite
.. code-block:: modula2
    PROCEDURE OpenToWrite (fname: String) : File ;
    
    
    (*
       OpenForRandom - attempts to open a file, fname, for random access
                       read or write and it returns this file.
                       The success of this operation can be checked by
                       calling IsNoError.
                       towrite, determines whether the file should be
                       opened for writing or reading.
                       if towrite is TRUE or whether the previous file should
                       be left alone, allowing this descriptor to seek
                       and modify an existing file.
    *)
    
.. index::
   OpenForRandom
.. code-block:: modula2
    PROCEDURE OpenForRandom (fname: String; towrite, newfile: BOOLEAN) : File ;
    
    
    (*
       WriteS - writes a string, s, to, file. It returns the String, s.
    *)
    
.. index::
   WriteS
.. code-block:: modula2
    PROCEDURE WriteS (file: File; s: String) : String ;
    
    
    (*
       ReadS - reads a string, s, from, file. It returns the String, s.
               It stops reading the string at the end of line or end of file.
               It consumes the newline at the end of line but does not place
               this into the returned string.
    *)
    
.. index::
   ReadS
.. code-block:: modula2
    PROCEDURE ReadS (file: File) : String ;
    
    
    END SFIO.

@c @node gm2-libs/SMathLib0, gm2-libs/SYSTEM, gm2-libs/SFIO, Base libraries
gm2-libs/SMathLib0
------------------

.. code-block:: modula2
    DEFINITION MODULE SMathLib0 ;

    CONST
       pi   = 3.1415926535897932384626433832795028841972;
       exp1 = 2.7182818284590452353602874713526624977572;
    
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (x: SHORTREAL) : SHORTREAL ;
.. index::
   ln
.. code-block:: modula2
    PROCEDURE ln (x: SHORTREAL) : SHORTREAL ;
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: SHORTREAL) : SHORTREAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: SHORTREAL) : SHORTREAL ;
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (x: SHORTREAL) : SHORTREAL ;
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (x: SHORTREAL) : SHORTREAL ;
.. index::
   entier
.. code-block:: modula2
    PROCEDURE entier (x: SHORTREAL) : INTEGER ;
    
    
    END SMathLib0.

@c @node gm2-libs/SYSTEM, gm2-libs/Scan, gm2-libs/SMathLib0, Base libraries
gm2-libs/SYSTEM
---------------

.. code-block:: modula2
    DEFINITION MODULE SYSTEM ;

    EXPORT QUALIFIED BITSPERBYTE, BYTESPERWORD,
                     ADDRESS, WORD, BYTE, CSIZE_T, CSSIZE_T, (* @SYSTEM_DATATYPES@  *)
                     ADR, TSIZE, ROTATE, SHIFT, THROW, TBITSIZE ;
                     (* SIZE is also exported if -fpim2 is used,  *)
    
    CONST
.. index::
   pair: BITSPERBYTE; (const)
.. code-block:: modula2
      BITSPERBYTE   = __ATTRIBUTE__ __BUILTIN__ ((BITS_PER_UNIT)) ;
.. index::
   pair: BYTESPERWORD; (const)
.. code-block:: modula2
      BYTESPERWORD  = __ATTRIBUTE__ __BUILTIN__ ((UNITS_PER_WORD)) ;
    
    
    (* all the following types are declared internally to gm2
    TYPE
       @SYSTEM_TYPES@
    *)
    
    
    (*
       all the functions below are declared internally to gm2
       ====================================================
    
.. index::
   ADR
.. code-block:: modula2
    PROCEDURE ADR (VAR v: <anytype>): ADDRESS;
      (* Returns the address of variable v. *)
    
.. index::
   SIZE
.. code-block:: modula2
    PROCEDURE SIZE (v: <type>) : ZType;
      (* Returns the number of BYTES used to store a v of
         any specified <type>.  Only available if -fpim2 is used.
      *)
    
.. index::
   TSIZE
.. code-block:: modula2
    PROCEDURE TSIZE (<type>) : CARDINAL;
      (* Returns the number of BYTES used to store a value of the
         specified <type>.
      *)
    
.. index::
   ROTATE
.. code-block:: modula2
    PROCEDURE ROTATE (val: <a set type>;
                      num: INTEGER): <type of first parameter>;
      (* Returns a bit sequence obtained from val by rotating up/right
         or down/right by the absolute value of num.  The direction is
         down/right if the sign of num is negative, otherwise the direction
         is up/left.
      *)
    
.. index::
   SHIFT
.. code-block:: modula2
    PROCEDURE SHIFT (val: <a set type>;
                     num: INTEGER): <type of first parameter>;
      (* Returns a bit sequence obtained from val by shifting up/left
         or down/right by the absolute value of num, introducing
         zeros as necessary.  The direction is down/right if the sign of
         num is negative, otherwise the direction is up/left.
      *)
    
.. index::
   THROW
.. code-block:: modula2
    PROCEDURE THROW (i: INTEGER) ;
      (*
         THROW is a GNU extension and was not part of the PIM or ISO
         standards.  It throws an exception which will be caught by the
         EXCEPT block (assuming it exists).  This is a compiler builtin
         function which interfaces to the GCC exception handling runtime
         system.
         GCC uses the term throw, hence the naming distinction between
         the GCC builtin and the Modula-2 runtime library procedure Raise.
         The later library procedure Raise will call SYSTEM.THROW after
         performing various housekeeping activities.
      *)
    
.. index::
   TBITSIZE
.. code-block:: modula2
    PROCEDURE TBITSIZE (<type>) : CARDINAL ;
      (* Returns the minimum number of bits necessary to represent
         <type>.  This procedure function is only useful for determining
         the number of bits used for any type field within a packed RECORD.
         It is not particularly useful elsewhere since <type> might be
         optimized for speed, for example a BOOLEAN could occupy a WORD.
      *)
    *)
    
    (* The following procedures are invoked by GNU Modula-2 to
       shift non word sized set types. They are not strictly part
       of the core PIM Modula-2, however they are used
       to implement the SHIFT procedure defined above,
       which are in turn used by the Logitech compatible libraries.
    
       Users will access these procedures by using the procedure
       SHIFT above and GNU Modula-2 will map SHIFT onto one of
       the following procedures.
    *)
    
    (*
       ShiftVal - is a runtime procedure whose job is to implement
                  the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
                  inline a SHIFT of a single WORD sized set and will only
                  call this routine for larger sets.
    *)
    
.. index::
   ShiftVal
.. code-block:: modula2
    PROCEDURE ShiftVal (VAR s, d: ARRAY OF BITSET;
                        SetSizeInBits: CARDINAL;
                        ShiftCount: INTEGER) ;
    
    
    (*
       ShiftLeft - performs the shift left for a multi word set.
                   This procedure might be called by the back end of
                   GNU Modula-2 depending whether amount is known at
                   compile time.
    *)
    
.. index::
   ShiftLeft
.. code-block:: modula2
    PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         ShiftCount: CARDINAL) ;
    
    (*
       ShiftRight - performs the shift left for a multi word set.
                    This procedure might be called by the back end of
                    GNU Modula-2 depending whether amount is known at
                    compile time.
    *)
    
.. index::
   ShiftRight
.. code-block:: modula2
    PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                          SetSizeInBits: CARDINAL;
                          ShiftCount: CARDINAL) ;
    
    
    (*
       RotateVal - is a runtime procedure whose job is to implement
                   the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
                   inline a ROTATE of a single WORD (or less)
                   sized set and will only call this routine for larger
                   sets.
    *)
    
.. index::
   RotateVal
.. code-block:: modula2
    PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         RotateCount: INTEGER) ;
    
    
    (*
       RotateLeft - performs the rotate left for a multi word set.
                    This procedure might be called by the back end of
                    GNU Modula-2 depending whether amount is known at
                    compile time.
    *)
    
.. index::
   RotateLeft
.. code-block:: modula2
    PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                          SetSizeInBits: CARDINAL;
                          RotateCount: CARDINAL) ;
    
    
    (*
       RotateRight - performs the rotate right for a multi word set.
                     This procedure might be called by the back end of
                     GNU Modula-2 depending whether amount is known at
                     compile time.
    *)
    
.. index::
   RotateRight
.. code-block:: modula2
    PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                           SetSizeInBits: CARDINAL;
                           RotateCount: CARDINAL) ;
    
    
    END SYSTEM.

@c @node gm2-libs/Scan, gm2-libs/Selective, gm2-libs/SYSTEM, Base libraries
gm2-libs/Scan
-------------

.. code-block:: modula2
    DEFINITION MODULE Scan ;

(* Provides a primitive symbol fetching from input.
   Symbols are delimited by spaces and tabs.
   Limitation only allows one source file at
   a time to deliver symbols.  *)
    
    
    EXPORT QUALIFIED GetNextSymbol, WriteError,
                     OpenSource, CloseSource,
                     TerminateOnError, DefineComments ;
    
    
    (* OpenSource - opens a source file for reading.                  *)
    
.. index::
   OpenSource
.. code-block:: modula2
    PROCEDURE OpenSource (a: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (* CloseSource - closes the current source file from reading.     *)
    
.. index::
   CloseSource
.. code-block:: modula2
    PROCEDURE CloseSource ;
    
    
    (* GetNextSymbol gets the next source symbol and returns it in a. *)
    
.. index::
   GetNextSymbol
.. code-block:: modula2
    PROCEDURE GetNextSymbol (VAR a: ARRAY OF CHAR) ;
    
    
    (* WriteError writes a message, a, under the source line, which   *)
    (* attempts to pinpoint the Symbol at fault.                      *)
    
.. index::
   WriteError
.. code-block:: modula2
    PROCEDURE WriteError (a: ARRAY OF CHAR) ;
    
    
    (*
       TerminateOnError - exits with status 1 if we call WriteError.
    *)
    
.. index::
   TerminateOnError
.. code-block:: modula2
    PROCEDURE TerminateOnError ;
    
    
    (*
       DefineComments - defines the start of comments within the source
                        file.
    
                        The characters in Start define the comment start
                        and characters in End define the end.
                        The BOOLEAN eoln determine whether the comment
                        is terminated by end of line. If eoln is TRUE
                        then End is ignored.
    
                        If this procedure is never called then no comments
                        are allowed.
    *)
    
.. index::
   DefineComments
.. code-block:: modula2
    PROCEDURE DefineComments (Start, End: ARRAY OF CHAR; eoln: BOOLEAN) ;
    
    
    END Scan.

@c @node gm2-libs/Selective, gm2-libs/StdIO, gm2-libs/Scan, Base libraries
gm2-libs/Selective
------------------

.. code-block:: modula2
    DEFINITION MODULE Selective ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    EXPORT QUALIFIED SetOfFd, Timeval,
                     InitSet, KillSet, InitTime, KillTime,
                     GetTime, SetTime,
                     FdZero, FdSet, FdClr, FdIsSet, Select,
                     MaxFdsPlusOne, WriteCharRaw, ReadCharRaw,
                     GetTimeOfDay ;
    
    TYPE
.. index::
   pair: SetOfFd; (type)
.. code-block:: modula2
       SetOfFd = ADDRESS ;    (* Hidden type in Selective.c *)
.. index::
   pair: Timeval; (type)
.. code-block:: modula2
       Timeval = ADDRESS ;    (* Hidden type in Selective.c *)
    
    
.. index::
   Select
.. code-block:: modula2
    PROCEDURE Select (nooffds: CARDINAL;
                      readfds, writefds, exceptfds: SetOfFd;
                      timeout: Timeval) : INTEGER ;
    
.. index::
   InitTime
.. code-block:: modula2
    PROCEDURE InitTime (sec, usec: CARDINAL) : Timeval ;
.. index::
   KillTime
.. code-block:: modula2
    PROCEDURE KillTime (t: Timeval) : Timeval ;
.. index::
   GetTime
.. code-block:: modula2
    PROCEDURE GetTime (t: Timeval; VAR sec, usec: CARDINAL) ;
.. index::
   SetTime
.. code-block:: modula2
    PROCEDURE SetTime (t: Timeval; sec, usec: CARDINAL) ;
.. index::
   InitSet
.. code-block:: modula2
    PROCEDURE InitSet () : SetOfFd ;
.. index::
   KillSet
.. code-block:: modula2
    PROCEDURE KillSet (s: SetOfFd) : SetOfFd ;
.. index::
   FdZero
.. code-block:: modula2
    PROCEDURE FdZero (s: SetOfFd) ;
.. index::
   FdSet
.. code-block:: modula2
    PROCEDURE FdSet (fd: INTEGER; s: SetOfFd) ;
.. index::
   FdClr
.. code-block:: modula2
    PROCEDURE FdClr (fd: INTEGER; s: SetOfFd) ;
.. index::
   FdIsSet
.. code-block:: modula2
    PROCEDURE FdIsSet (fd: INTEGER; s: SetOfFd) : BOOLEAN ;
.. index::
   MaxFdsPlusOne
.. code-block:: modula2
    PROCEDURE MaxFdsPlusOne (a, b: INTEGER) : INTEGER ;
    
    (* you must use the raw routines with select - not the FIO buffered routines *)
.. index::
   WriteCharRaw
.. code-block:: modula2
    PROCEDURE WriteCharRaw (fd: INTEGER; ch: CHAR) ;
.. index::
   ReadCharRaw
.. code-block:: modula2
    PROCEDURE ReadCharRaw (fd: INTEGER) : CHAR ;
    
    (*
       GetTimeOfDay - fills in a record, Timeval, filled in with the
                      current system time in seconds and microseconds.
                      It returns zero (see man 3p gettimeofday)
    *)
    
.. index::
   GetTimeOfDay
.. code-block:: modula2
    PROCEDURE GetTimeOfDay (tv: Timeval) : INTEGER ;
    
    
    END Selective.

@c @node gm2-libs/StdIO, gm2-libs/Storage, gm2-libs/Selective, Base libraries
gm2-libs/StdIO
--------------

.. code-block:: modula2
    DEFINITION MODULE StdIO ;

    EXPORT QUALIFIED ProcRead, ProcWrite,
                     Read, Write,
                     PushOutput, PopOutput, GetCurrentOutput,
                     PushInput, PopInput, GetCurrentInput ;
    
    
    TYPE
.. index::
   pair: ProcWrite; (type)
.. code-block:: modula2
       ProcWrite = PROCEDURE (CHAR) ;
.. index::
   pair: ProcRead; (type)
.. code-block:: modula2
       ProcRead  = PROCEDURE (VAR CHAR) ;
    
    
    (*
       Read - is the generic procedure that all higher application layers
              should use to receive a character.
    *)
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR ch: CHAR) ;
    
    
    (*
       Write - is the generic procedure that all higher application layers
               should use to emit a character.
    *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (ch: CHAR) ;
    
    
    (*
       PushOutput - pushes the current Write procedure onto a stack,
                    any future references to Write will actually invoke
                    procedure, p.
    *)
    
.. index::
   PushOutput
.. code-block:: modula2
    PROCEDURE PushOutput (p: ProcWrite) ;
    
    
    (*
       PopOutput - restores Write to use the previous output procedure.
    *)
    
.. index::
   PopOutput
.. code-block:: modula2
    PROCEDURE PopOutput ;
    
    
    (*
       GetCurrentOutput - returns the current output procedure.
    *)
    
.. index::
   GetCurrentOutput
.. code-block:: modula2
    PROCEDURE GetCurrentOutput () : ProcWrite ;
    
    
    (*
       PushInput - pushes the current Read procedure onto a stack,
                   any future references to Read will actually invoke
                   procedure, p.
    *)
    
.. index::
   PushInput
.. code-block:: modula2
    PROCEDURE PushInput (p: ProcRead) ;
    
    
    (*
       PopInput - restores Write to use the previous output procedure.
    *)
    
.. index::
   PopInput
.. code-block:: modula2
    PROCEDURE PopInput ;
    
    
    (*
       GetCurrentInput - returns the current input procedure.
    *)
    
.. index::
   GetCurrentInput
.. code-block:: modula2
    PROCEDURE GetCurrentInput () : ProcRead ;
    
    
    END StdIO.

@c @node gm2-libs/Storage, gm2-libs/StrCase, gm2-libs/StdIO, Base libraries
gm2-libs/Storage
----------------

.. code-block:: modula2
    DEFINITION MODULE Storage ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    EXPORT QUALIFIED ALLOCATE, DEALLOCATE, REALLOCATE, Available ;
    
    
    
    (*
       ALLOCATE - attempt to allocate memory from the heap.
                  NIL is returned in, a, if ALLOCATE fails.
    *)
    
.. index::
   ALLOCATE
.. code-block:: modula2
    PROCEDURE ALLOCATE (VAR a: ADDRESS ; Size: CARDINAL) ;
    
    
    (*
       DEALLOCATE - return, Size, bytes to the heap.
                    The variable, a, is set to NIL.
    *)
    
.. index::
   DEALLOCATE
.. code-block:: modula2
    PROCEDURE DEALLOCATE (VAR a: ADDRESS ; Size: CARDINAL) ;
    
    
    (*
       REALLOCATE - attempts to reallocate storage. The address,
                    a, should either be NIL in which case ALLOCATE
                    is called, or alternatively it should have already
                    been initialized by ALLOCATE. The allocated storage
                    is resized accordingly.
    *)
    
.. index::
   REALLOCATE
.. code-block:: modula2
    PROCEDURE REALLOCATE (VAR a: ADDRESS; Size: CARDINAL) ;
    
    
    (*
       Available - returns TRUE if, Size, bytes can be allocated.
    *)
    
.. index::
   Available
.. code-block:: modula2
    PROCEDURE Available (Size: CARDINAL) : BOOLEAN ;
    
    
    END Storage.

@c @node gm2-libs/StrCase, gm2-libs/StrIO, gm2-libs/Storage, Base libraries
gm2-libs/StrCase
----------------

.. code-block:: modula2
    DEFINITION MODULE StrCase ;

    
    EXPORT QUALIFIED StrToUpperCase, StrToLowerCase, Cap, Lower ;
    
    
    (*
       StrToUpperCase - converts string, a, to uppercase returning the
                        result in, b.
    *)
    
.. index::
   StrToUpperCase
.. code-block:: modula2
    PROCEDURE StrToUpperCase (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;
    
    
    (*
       StrToLowerCase - converts string, a, to lowercase returning the
                        result in, b.
    *)
    
.. index::
   StrToLowerCase
.. code-block:: modula2
    PROCEDURE StrToLowerCase (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;
    
    
    (*
       Cap - converts a lower case character into a capital character.
             If the character is not a lower case character 'a'..'z'
             then the character is simply returned unaltered.
    *)
    
.. index::
   Cap
.. code-block:: modula2
    PROCEDURE Cap (ch: CHAR) : CHAR ;
    
    
    (*
       Lower - converts an upper case character into a lower case character.
               If the character is not an upper case character 'A'..'Z'
               then the character is simply returned unaltered.
    *)
    
.. index::
   Lower
.. code-block:: modula2
    PROCEDURE Lower (ch: CHAR) : CHAR ;
    
    
    END StrCase.

@c @node gm2-libs/StrIO, gm2-libs/StrLib, gm2-libs/StrCase, Base libraries
gm2-libs/StrIO
--------------

.. code-block:: modula2
    DEFINITION MODULE StrIO ;

    EXPORT QUALIFIED ReadString, WriteString,
                     WriteLn ;
    
    
    (*
       WriteLn - writes a carriage return and a newline
                 character.
    *)
    
.. index::
   WriteLn
.. code-block:: modula2
    PROCEDURE WriteLn ;
    
    
    (*
       ReadString - reads a sequence of characters into a string.
                    Line editing accepts Del, Ctrl H, Ctrl W and
                    Ctrl U.
    *)
    
.. index::
   ReadString
.. code-block:: modula2
    PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ;
    
    
    (*
       WriteString - writes a string to the default output.
    *)
    
.. index::
   WriteString
.. code-block:: modula2
    PROCEDURE WriteString (a: ARRAY OF CHAR) ;
    
    
    END StrIO.

@c @node gm2-libs/StrLib, gm2-libs/StringConvert, gm2-libs/StrIO, Base libraries
gm2-libs/StrLib
---------------

.. code-block:: modula2
    DEFINITION MODULE StrLib ;

    EXPORT QUALIFIED StrConCat, StrLen, StrCopy, StrEqual, StrLess,
          	       	 IsSubString, StrRemoveWhitePrefix ;
    
    
    (*
       StrConCat - combines a and b into c.
    *)
    
.. index::
   StrConCat
.. code-block:: modula2
    PROCEDURE StrConCat (a, b: ARRAY OF CHAR; VAR c: ARRAY OF CHAR) ;
    
    
    (*
       StrLess - returns TRUE if string, a, alphabetically occurs before
                 string, b.
    *)
    
.. index::
   StrLess
.. code-block:: modula2
    PROCEDURE StrLess (a, b: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       StrEqual - performs a = b on two strings.
    *)
    
.. index::
   StrEqual
.. code-block:: modula2
    PROCEDURE StrEqual (a, b: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       StrLen - returns the length of string, a.
    *)
    
.. index::
   StrLen
.. code-block:: modula2
    PROCEDURE StrLen (a: ARRAY OF CHAR) : CARDINAL ;
    
    
    (*
       StrCopy - copy string src into string dest providing dest is large enough.
                 If dest is smaller than a then src then the string is truncated when
                 dest is full.  Add a nul character if there is room in dest.
    *)
    
.. index::
   StrCopy
.. code-block:: modula2
    PROCEDURE StrCopy (src: ARRAY OF CHAR ; VAR dest: ARRAY OF CHAR) ;
    
    
    (*
       IsSubString - returns true if b is a subcomponent of a.
    *)
    
.. index::
   IsSubString
.. code-block:: modula2
    PROCEDURE IsSubString (a, b: ARRAY OF CHAR) : BOOLEAN ;
    
    
    (*
       StrRemoveWhitePrefix - copies string, into string, b, excluding any white
                              space infront of a.
    *)
    
.. index::
   StrRemoveWhitePrefix
.. code-block:: modula2
    PROCEDURE StrRemoveWhitePrefix (a: ARRAY OF CHAR; VAR b: ARRAY OF CHAR) ;
    
    
    END StrLib.

@c @node gm2-libs/StringConvert, gm2-libs/SysExceptions, gm2-libs/StrLib, Base libraries
gm2-libs/StringConvert
----------------------

.. code-block:: modula2
    DEFINITION MODULE StringConvert ;

    FROM DynamicStrings IMPORT String ;
    EXPORT QUALIFIED IntegerToString, StringToInteger,
                     StringToLongInteger, LongIntegerToString,
                     StringToCardinal, CardinalToString,
                     StringToLongCardinal, LongCardinalToString,
                     StringToShortCardinal, ShortCardinalToString,
                     StringToLongreal, LongrealToString,
                     ToSigFig,
                     stoi, itos, ctos, stoc, hstoi, ostoi, bstoi,
                     hstoc, ostoc, bstoc,
                     stor, stolr ;
    
    
    (*
       IntegerToString - converts INTEGER, i, into a String. The field with
                         can be specified if non zero. Leading characters
                         are defined by padding and this function will
                         prepend a + if sign is set to TRUE.
                         The base allows the caller to generate binary,
                         octal, decimal, hexidecimal numbers.
                         The value of lower is only used when hexidecimal
                         numbers are generated and if TRUE then digits
                         abcdef are used, and if FALSE then ABCDEF are used.
    *)
    
.. index::
   IntegerToString
.. code-block:: modula2
    PROCEDURE IntegerToString (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN;
                               base: CARDINAL; lower: BOOLEAN) : String ;
    
    
    (*
       CardinalToString - converts CARDINAL, c, into a String. The field
                          width can be specified if non zero. Leading
                          characters are defined by padding.
                          The base allows the caller to generate binary,
                          octal, decimal, hexidecimal numbers.
                          The value of lower is only used when hexidecimal
                          numbers are generated and if TRUE then digits
                          abcdef are used, and if FALSE then ABCDEF are used.
    *)
    
.. index::
   CardinalToString
.. code-block:: modula2
    PROCEDURE CardinalToString (c: CARDINAL; width: CARDINAL; padding: CHAR;
                                base: CARDINAL; lower: BOOLEAN) : String ;
    
    
    (*
       StringToInteger - converts a string, s, of, base, into an INTEGER.
                         Leading white space is ignored. It stops converting
                         when either the string is exhausted or if an illegal
                         numeral is found.
                         The parameter found is set TRUE if a number was found.
    *)
    
.. index::
   StringToInteger
.. code-block:: modula2
    PROCEDURE StringToInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : INTEGER ;
    
    
    (*
       StringToCardinal - converts a string, s, of, base, into a CARDINAL.
                          Leading white space is ignored. It stops converting
                          when either the string is exhausted or if an illegal
                          numeral is found.
                          The parameter found is set TRUE if a number was found.
    *)
    
.. index::
   StringToCardinal
.. code-block:: modula2
    PROCEDURE StringToCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : CARDINAL ;
    
    
    (*
       LongIntegerToString - converts LONGINT, i, into a String. The field with
                             can be specified if non zero. Leading characters
                             are defined by padding and this function will
                             prepend a + if sign is set to TRUE.
                             The base allows the caller to generate binary,
                             octal, decimal, hexidecimal numbers.
                             The value of lower is only used when hexidecimal
                             numbers are generated and if TRUE then digits
                             abcdef are used, and if FALSE then ABCDEF are used.
    *)
    
.. index::
   LongIntegerToString
.. code-block:: modula2
    PROCEDURE LongIntegerToString (i: LONGINT; width: CARDINAL; padding: CHAR;
                                   sign: BOOLEAN; base: CARDINAL; lower: BOOLEAN) : String ;
    
    
    
    (*
       StringToLongInteger - converts a string, s, of, base, into an LONGINT.
                             Leading white space is ignored. It stops converting
                             when either the string is exhausted or if an illegal
                             numeral is found.
                             The parameter found is set TRUE if a number was found.
    *)
    
.. index::
   StringToLongInteger
.. code-block:: modula2
    PROCEDURE StringToLongInteger (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGINT ;
    
    
    (*
       LongCardinalToString - converts LONGCARD, c, into a String. The field
                              width can be specified if non zero. Leading
                              characters are defined by padding.
                              The base allows the caller to generate binary,
                              octal, decimal, hexidecimal numbers.
                              The value of lower is only used when hexidecimal
                              numbers are generated and if TRUE then digits
                              abcdef are used, and if FALSE then ABCDEF are used.
    *)
    
.. index::
   LongCardinalToString
.. code-block:: modula2
    PROCEDURE LongCardinalToString (c: LONGCARD; width: CARDINAL; padding: CHAR;
                                    base: CARDINAL; lower: BOOLEAN) : String ;
    
    
    (*
       StringToLongCardinal - converts a string, s, of, base, into a LONGCARD.
                              Leading white space is ignored. It stops converting
                              when either the string is exhausted or if an illegal
                              numeral is found.
                              The parameter found is set TRUE if a number was found.
    *)
    
.. index::
   StringToLongCardinal
.. code-block:: modula2
    PROCEDURE StringToLongCardinal (s: String; base: CARDINAL; VAR found: BOOLEAN) : LONGCARD ;
    
    
    (*
       ShortCardinalToString - converts SHORTCARD, c, into a String. The field
                               width can be specified if non zero. Leading
                               characters are defined by padding.
                               The base allows the caller to generate binary,
                               octal, decimal, hexidecimal numbers.
                               The value of lower is only used when hexidecimal
                               numbers are generated and if TRUE then digits
                               abcdef are used, and if FALSE then ABCDEF are used.
    *)
    
.. index::
   ShortCardinalToString
.. code-block:: modula2
    PROCEDURE ShortCardinalToString (c: SHORTCARD; width: CARDINAL; padding: CHAR;
                                     base: CARDINAL; lower: BOOLEAN) : String ;
    
    
    (*
       StringToShortCardinal - converts a string, s, of, base, into a SHORTCARD.
                               Leading white space is ignored. It stops converting
                               when either the string is exhausted or if an illegal
                               numeral is found.
                               The parameter found is set TRUE if a number was found.
    *)
    
.. index::
   StringToShortCardinal
.. code-block:: modula2
    PROCEDURE StringToShortCardinal (s: String; base: CARDINAL;
                                     VAR found: BOOLEAN) : SHORTCARD ;
    
    
    (*
       stoi - decimal string to INTEGER
    *)
    
.. index::
   stoi
.. code-block:: modula2
    PROCEDURE stoi (s: String) : INTEGER ;
    
    
    (*
       itos - integer to decimal string.
    *)
    
.. index::
   itos
.. code-block:: modula2
    PROCEDURE itos (i: INTEGER; width: CARDINAL; padding: CHAR; sign: BOOLEAN) : String ;
    
    
    (*
       ctos - cardinal to decimal string.
    *)
    
.. index::
   ctos
.. code-block:: modula2
    PROCEDURE ctos (c: CARDINAL; width: CARDINAL; padding: CHAR) : String ;
    
    
    (*
       stoc - decimal string to CARDINAL
    *)
    
.. index::
   stoc
.. code-block:: modula2
    PROCEDURE stoc (s: String) : CARDINAL ;
    
    
    (*
       hstoi - hexidecimal string to INTEGER
    *)
    
.. index::
   hstoi
.. code-block:: modula2
    PROCEDURE hstoi (s: String) : INTEGER ;
    
    
    (*
       ostoi - octal string to INTEGER
    *)
    
.. index::
   ostoi
.. code-block:: modula2
    PROCEDURE ostoi (s: String) : INTEGER ;
    
    
    (*
       bstoi - binary string to INTEGER
    *)
    
.. index::
   bstoi
.. code-block:: modula2
    PROCEDURE bstoi (s: String) : INTEGER ;
    
    
    (*
       hstoc - hexidecimal string to CARDINAL
    *)
    
.. index::
   hstoc
.. code-block:: modula2
    PROCEDURE hstoc (s: String) : CARDINAL ;
    
    
    (*
       ostoc - octal string to CARDINAL
    *)
    
.. index::
   ostoc
.. code-block:: modula2
    PROCEDURE ostoc (s: String) : CARDINAL ;
    
    
    (*
       bstoc - binary string to CARDINAL
    *)
    
.. index::
   bstoc
.. code-block:: modula2
    PROCEDURE bstoc (s: String) : CARDINAL ;
    
    
    (*
       StringToLongreal - returns a LONGREAL and sets found to TRUE
                          if a legal number is seen.
    *)
    
.. index::
   StringToLongreal
.. code-block:: modula2
    PROCEDURE StringToLongreal (s: String; VAR found: BOOLEAN) : LONGREAL ;
    
    
    (*
       LongrealToString - converts a LONGREAL number, Real, which has,
                          TotalWidth, and FractionWidth into a string.
    
                          So for example:
    
                          LongrealToString(1.0, 4, 2)  -> '1.00'
                          LongrealToString(12.3, 5, 2) -> '12.30'
                          LongrealToString(12.3, 6, 2) -> ' 12.30'
                          LongrealToString(12.3, 6, 3) -> '12.300'
    
                          if total width is too small then the fraction
                          becomes truncated.
    
                          LongrealToString(12.3, 5, 3) -> '12.30'
    
                          If TotalWidth is 0 then the function
                          will return the value of x which is converted
                          into as a fixed point number with exhaustive
                          precision.
    *)
    
.. index::
   LongrealToString
.. code-block:: modula2
    PROCEDURE LongrealToString (x: LONGREAL;
                                TotalWidth, FractionWidth: CARDINAL) : String ;
    
    
    (*
       stor - returns a REAL given a string.
    *)
    
.. index::
   stor
.. code-block:: modula2
    PROCEDURE stor (s: String) : REAL ;
    
    
    (*
       stolr - returns a LONGREAL given a string.
    *)
    
.. index::
   stolr
.. code-block:: modula2
    PROCEDURE stolr (s: String) : LONGREAL ;
    
    
    (*
       ToSigFig - returns a floating point or base 10 integer
                  string which is accurate to, n, significant
                  figures.  It will return a new String
                  and, s, will be destroyed.
    
    
                  So:  12.345
    
                  rounded to the following significant figures yields
    
                  5      12.345
                  4      12.34
                  3      12.3
                  2      12
                  1      10
    *)
    
.. index::
   ToSigFig
.. code-block:: modula2
    PROCEDURE ToSigFig (s: String; n: CARDINAL) : String ;
    
    
    (*
       ToDecimalPlaces - returns a floating point or base 10 integer
                         string which is accurate to, n, decimal
                         places.  It will return a new String
                         and, s, will be destroyed.
                         Decimal places yields, n, digits after
                         the .
    
                         So:  12.345
    
                         rounded to the following decimal places yields
    
                         5      12.34500
                         4      12.3450
                         3      12.345
                         2      12.34
                         1      12.3
    *)
    
.. index::
   ToDecimalPlaces
.. code-block:: modula2
    PROCEDURE ToDecimalPlaces (s: String; n: CARDINAL) : String ;
    
    
    END StringConvert.

@c @node gm2-libs/SysExceptions, gm2-libs/SysStorage, gm2-libs/StringConvert, Base libraries
gm2-libs/SysExceptions
----------------------

.. code-block:: modula2
    DEFINITION MODULE SysExceptions ;

(* Provides a mechanism for the underlying libraries to
   configure the exception routines.  This mechanism
   is used by both the ISO and PIM libraries.
   It is written to be ISO compliant and this also
   allows for mixed dialect projects.  *)
    
    FROM SYSTEM IMPORT ADDRESS ;
    
    TYPE
.. index::
   pair: PROCEXCEPTION; (type)
.. code-block:: modula2
       PROCEXCEPTION = PROCEDURE (ADDRESS) ;
    
.. index::
   InitExceptionHandlers
.. code-block:: modula2
    PROCEDURE InitExceptionHandlers (indexf, range, casef, invalidloc,
                                     function, wholevalue, wholediv,
                                     realvalue, realdiv, complexvalue,
                                     complexdiv, protection, systemf,
                                     coroutine, exception: PROCEXCEPTION) ;
    
    
    END SysExceptions.

@c @node gm2-libs/SysStorage, gm2-libs/TimeString, gm2-libs/SysExceptions, Base libraries
gm2-libs/SysStorage
-------------------

.. code-block:: modula2
    DEFINITION MODULE SysStorage ;

(*  Provides dynamic allocation for the system components.
    This allows the application to use the traditional Storage module
    which can be handled differently.  *)
    
    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT QUALIFIED ALLOCATE, DEALLOCATE, REALLOCATE, Available, Init ;
    
    
    (*
       ALLOCATE - attempt to allocate memory from the heap.
                  NIL is returned in, a, if ALLOCATE fails.
    *)
    
.. index::
   ALLOCATE
.. code-block:: modula2
    PROCEDURE ALLOCATE (VAR a: ADDRESS ; size: CARDINAL) ;
    
    
    (*
       DEALLOCATE - return, size, bytes to the heap.
                    The variable, a, is set to NIL.
    *)
    
.. index::
   DEALLOCATE
.. code-block:: modula2
    PROCEDURE DEALLOCATE (VAR a: ADDRESS ; size: CARDINAL) ;
    
    
    (*
       REALLOCATE - attempts to reallocate storage. The address,
                    a, should either be NIL in which case ALLOCATE
                    is called, or alternatively it should have already
                    been initialized by ALLOCATE. The allocated storage
                    is resized accordingly.
    *)
    
.. index::
   REALLOCATE
.. code-block:: modula2
    PROCEDURE REALLOCATE (VAR a: ADDRESS; size: CARDINAL) ;
    
    
    (*
       Available - returns TRUE if, size, bytes can be allocated.
    *)
    
.. index::
   Available
.. code-block:: modula2
    PROCEDURE Available (size: CARDINAL) : BOOLEAN;
    
    
    (*
       Init - initializes the heap.
              This does nothing on a GNU/Linux system.
              But it remains here since it might be used in an
              embedded system.
    *)
    
.. index::
   Init
.. code-block:: modula2
    PROCEDURE Init ;
    
    
    END SysStorage.

@c @node gm2-libs/TimeString, gm2-libs/UnixArgs, gm2-libs/SysStorage, Base libraries
gm2-libs/TimeString
-------------------

.. code-block:: modula2
    DEFINITION MODULE TimeString ;

    EXPORT QUALIFIED GetTimeString ;
    
    
    (*
       GetTimeString - places the time in ascii format into array, a.
    
    *)
    
.. index::
   GetTimeString
.. code-block:: modula2
    PROCEDURE GetTimeString (VAR a: ARRAY OF CHAR) ;
    
    
    END TimeString.

@c @node gm2-libs/UnixArgs, gm2-libs/cbuiltin, gm2-libs/TimeString, Base libraries
gm2-libs/UnixArgs
-----------------

.. code-block:: modula2
    DEFINITION MODULE UnixArgs ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    EXPORT QUALIFIED GetArgC, GetArgV, GetEnvV ;
    
.. index::
   GetArgC
.. code-block:: modula2
    PROCEDURE GetArgC () : INTEGER ;
.. index::
   GetArgV
.. code-block:: modula2
    PROCEDURE GetArgV () : ADDRESS ;
.. index::
   GetEnvV
.. code-block:: modula2
    PROCEDURE GetEnvV () : ADDRESS ;
    
    
    END UnixArgs.

@c @node gm2-libs/cbuiltin, gm2-libs/cgetopt, gm2-libs/UnixArgs, Base libraries
gm2-libs/cbuiltin
-----------------

.. code-block:: modula2
    DEFINITION MODULE FOR "C" cbuiltin ;

    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT UNQUALIFIED alloca, memcpy,
    		   isfinite, isfinitef, isfinitel,
    		   isinf_sign, isinf_signf, isinf_signl,
                       sinf, sinl, sin,
                       cosf, cosl, cos,
                       atan2f, atan2l, atan2,
                       sqrtf, sqrtl, sqrt,
                       fabsf, fabsl, fabs,
                       logf, logl, log,
                       expf, expl, exp,
                       log10f, log10l, log10,
                       exp10f, exp10l, exp10,
                       ilogbf, ilogbl, ilogb,
                       significand, significandf, significandl,
                       modf, modff, modfl,
                       nextafter, nextafterf, nextafterl,
                       nexttoward, nexttowardf, nexttowardl,
                       scalb, scalbf, scalbl,
                       scalbn, scalbnf, scalbnl,
                       scalbln, scalblnf, scalblnl,
    
                       cabsf, cabsl, cabs,
                       cargf, carg, cargl,
                       conjf, conj, conjl,
                       cpowf, cpow, cpowl,
                       csqrtf, csqrt, csqrtl,
                       cexpf, cexp, cexpl,
                       clogf, clog, clogl,
                       csinf, csin, csinl,
                       ccosf, ccos, ccosl,
                       ctanf, ctan, ctanl,
                       casinf, casin, casinl,
                       cacosf, cacos, cacosl,
                       catanf, catan, catanl,
    
                       index, rindex,
                       memcmp, memset, memmove,
                       strcat, strncat, strcpy, strncpy, strcmp, strncmp,
                       strlen, strstr, strpbrk, strspn, strcspn, strchr, strrchr ;
    
.. index::
   alloca
.. code-block:: modula2
    PROCEDURE alloca (i: CARDINAL) : ADDRESS ;
.. index::
   memcpy
.. code-block:: modula2
    PROCEDURE memcpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
.. index::
   isfinite
.. code-block:: modula2
    PROCEDURE isfinite (x: REAL) : BOOLEAN ;
.. index::
   isfinitel
.. code-block:: modula2
    PROCEDURE isfinitel (x: LONGREAL) : BOOLEAN ;
.. index::
   isfinitef
.. code-block:: modula2
    PROCEDURE isfinitef (x: SHORTREAL) : BOOLEAN ;
.. index::
   isinf_sign
.. code-block:: modula2
    PROCEDURE isinf_sign (x: REAL) : BOOLEAN ;
.. index::
   isinf_signl
.. code-block:: modula2
    PROCEDURE isinf_signl (x: LONGREAL) : BOOLEAN ;
.. index::
   isinf_signf
.. code-block:: modula2
    PROCEDURE isinf_signf (x: SHORTREAL) : BOOLEAN ;
.. index::
   sinf
.. code-block:: modula2
    PROCEDURE sinf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sin
.. code-block:: modula2
    PROCEDURE sin (x: REAL) : REAL ;
.. index::
   sinl
.. code-block:: modula2
    PROCEDURE sinl (x: LONGREAL) : LONGREAL ;
.. index::
   cosf
.. code-block:: modula2
    PROCEDURE cosf (x: SHORTREAL) : SHORTREAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE cos (x: REAL) : REAL ;
.. index::
   cosl
.. code-block:: modula2
    PROCEDURE cosl (x: LONGREAL) : LONGREAL ;
.. index::
   atan2f
.. code-block:: modula2
    PROCEDURE atan2f (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   atan2
.. code-block:: modula2
    PROCEDURE atan2 (x, y: REAL) : REAL ;
.. index::
   atan2l
.. code-block:: modula2
    PROCEDURE atan2l (x, y: LONGREAL) : LONGREAL ;
.. index::
   sqrtf
.. code-block:: modula2
    PROCEDURE sqrtf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE sqrt (x: REAL) : REAL ;
.. index::
   sqrtl
.. code-block:: modula2
    PROCEDURE sqrtl (x: LONGREAL) : LONGREAL ;
.. index::
   fabsf
.. code-block:: modula2
    PROCEDURE fabsf (x: SHORTREAL) : SHORTREAL ;
.. index::
   fabs
.. code-block:: modula2
    PROCEDURE fabs (x: REAL) : REAL ;
.. index::
   fabsl
.. code-block:: modula2
    PROCEDURE fabsl (x: LONGREAL) : LONGREAL ;
.. index::
   logf
.. code-block:: modula2
    PROCEDURE logf (x: SHORTREAL) : SHORTREAL ;
.. index::
   log
.. code-block:: modula2
    PROCEDURE log (x: REAL) : REAL ;
.. index::
   logl
.. code-block:: modula2
    PROCEDURE logl (x: LONGREAL) : LONGREAL ;
.. index::
   expf
.. code-block:: modula2
    PROCEDURE expf (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (x: REAL) : REAL ;
.. index::
   expl
.. code-block:: modula2
    PROCEDURE expl (x: LONGREAL) : LONGREAL ;
.. index::
   log10f
.. code-block:: modula2
    PROCEDURE log10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   log10
.. code-block:: modula2
    PROCEDURE log10 (x: REAL) : REAL ;
.. index::
   log10l
.. code-block:: modula2
    PROCEDURE log10l (x: LONGREAL) : LONGREAL ;
.. index::
   exp10f
.. code-block:: modula2
    PROCEDURE exp10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp10
.. code-block:: modula2
    PROCEDURE exp10 (x: REAL) : REAL ;
.. index::
   exp10l
.. code-block:: modula2
    PROCEDURE exp10l (x: LONGREAL) : LONGREAL ;
.. index::
   ilogbf
.. code-block:: modula2
    PROCEDURE ilogbf (x: SHORTREAL) : INTEGER ;
.. index::
   ilogb
.. code-block:: modula2
    PROCEDURE ilogb (x: REAL) : INTEGER ;
.. index::
   ilogbl
.. code-block:: modula2
    PROCEDURE ilogbl (x: LONGREAL) : INTEGER ;
    
.. index::
   significand
.. code-block:: modula2
    PROCEDURE significand (r: REAL) : REAL ;
.. index::
   significandf
.. code-block:: modula2
    PROCEDURE significandf (s: SHORTREAL) : SHORTREAL ;
.. index::
   significandl
.. code-block:: modula2
    PROCEDURE significandl (l: LONGREAL) : LONGREAL ;
    
.. index::
   modf
.. code-block:: modula2
    PROCEDURE modf (x: REAL; VAR y: REAL) : REAL ;
.. index::
   modff
.. code-block:: modula2
    PROCEDURE modff (x: SHORTREAL; VAR y: SHORTREAL) : SHORTREAL ;
.. index::
   modfl
.. code-block:: modula2
    PROCEDURE modfl (x: LONGREAL; VAR y: LONGREAL) : LONGREAL ;
    
.. index::
   nextafter
.. code-block:: modula2
    PROCEDURE nextafter (x, y: REAL) : REAL ;
.. index::
   nextafterf
.. code-block:: modula2
    PROCEDURE nextafterf (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   nextafterl
.. code-block:: modula2
    PROCEDURE nextafterl (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   nexttoward
.. code-block:: modula2
    PROCEDURE nexttoward (x, y: REAL) : REAL ;
.. index::
   nexttowardf
.. code-block:: modula2
    PROCEDURE nexttowardf (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   nexttowardl
.. code-block:: modula2
    PROCEDURE nexttowardl (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   scalb
.. code-block:: modula2
    PROCEDURE scalb (x, n: REAL) : REAL ;
.. index::
   scalbf
.. code-block:: modula2
    PROCEDURE scalbf (x, n: SHORTREAL) : SHORTREAL ;
.. index::
   scalbl
.. code-block:: modula2
    PROCEDURE scalbl (x, n: LONGREAL) : LONGREAL ;
    
.. index::
   scalbn
.. code-block:: modula2
    PROCEDURE scalbn (x: REAL; n: INTEGER) : REAL ;
.. index::
   scalbnf
.. code-block:: modula2
    PROCEDURE scalbnf (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
.. index::
   scalbnl
.. code-block:: modula2
    PROCEDURE scalbnl (x: LONGREAL; n: INTEGER) : LONGREAL ;
    
.. index::
   scalbln
.. code-block:: modula2
    PROCEDURE scalbln (x: REAL; n: LONGINT) : REAL ;
.. index::
   scalblnf
.. code-block:: modula2
    PROCEDURE scalblnf (x: SHORTREAL; n: LONGINT) : SHORTREAL ;
.. index::
   scalblnl
.. code-block:: modula2
    PROCEDURE scalblnl (x: LONGREAL; n: LONGINT) : LONGREAL ;
    
.. index::
   cabsf
.. code-block:: modula2
    PROCEDURE cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
.. index::
   cabs
.. code-block:: modula2
    PROCEDURE cabs (z: COMPLEX) : REAL ;
.. index::
   cabsl
.. code-block:: modula2
    PROCEDURE cabsl (z: LONGCOMPLEX) : LONGREAL ;
    
.. index::
   cargf
.. code-block:: modula2
    PROCEDURE cargf (z: SHORTCOMPLEX) : SHORTREAL ;
.. index::
   carg
.. code-block:: modula2
    PROCEDURE carg (z: COMPLEX) : REAL ;
.. index::
   cargl
.. code-block:: modula2
    PROCEDURE cargl (z: LONGCOMPLEX) : LONGREAL ;
    
.. index::
   conjf
.. code-block:: modula2
    PROCEDURE conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   conj
.. code-block:: modula2
    PROCEDURE conj (z: COMPLEX) : COMPLEX ;
.. index::
   conjl
.. code-block:: modula2
    PROCEDURE conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cpowf
.. code-block:: modula2
    PROCEDURE cpowf (base: SHORTCOMPLEX; exp: SHORTREAL) : SHORTCOMPLEX ;
.. index::
   cpow
.. code-block:: modula2
    PROCEDURE cpow (base: COMPLEX; exp: REAL) : COMPLEX ;
.. index::
   cpowl
.. code-block:: modula2
    PROCEDURE cpowl (base: LONGCOMPLEX; exp: LONGREAL) : LONGCOMPLEX ;
    
.. index::
   csqrtf
.. code-block:: modula2
    PROCEDURE csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   csqrt
.. code-block:: modula2
    PROCEDURE csqrt (z: COMPLEX) : COMPLEX ;
.. index::
   csqrtl
.. code-block:: modula2
    PROCEDURE csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cexpf
.. code-block:: modula2
    PROCEDURE cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   cexp
.. code-block:: modula2
    PROCEDURE cexp (z: COMPLEX) : COMPLEX ;
.. index::
   cexpl
.. code-block:: modula2
    PROCEDURE cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   clogf
.. code-block:: modula2
    PROCEDURE clogf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   clog
.. code-block:: modula2
    PROCEDURE clog (z: COMPLEX) : COMPLEX ;
.. index::
   clogl
.. code-block:: modula2
    PROCEDURE clogl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   csinf
.. code-block:: modula2
    PROCEDURE csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   csin
.. code-block:: modula2
    PROCEDURE csin (z: COMPLEX) : COMPLEX ;
.. index::
   csinl
.. code-block:: modula2
    PROCEDURE csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   ccosf
.. code-block:: modula2
    PROCEDURE ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   ccos
.. code-block:: modula2
    PROCEDURE ccos (z: COMPLEX) : COMPLEX ;
.. index::
   ccosl
.. code-block:: modula2
    PROCEDURE ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   ctanf
.. code-block:: modula2
    PROCEDURE ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   ctan
.. code-block:: modula2
    PROCEDURE ctan (z: COMPLEX) : COMPLEX ;
.. index::
   ctanl
.. code-block:: modula2
    PROCEDURE ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   casinf
.. code-block:: modula2
    PROCEDURE casinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   casin
.. code-block:: modula2
    PROCEDURE casin (z: COMPLEX) : COMPLEX ;
.. index::
   casinl
.. code-block:: modula2
    PROCEDURE casinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cacosf
.. code-block:: modula2
    PROCEDURE cacosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   cacos
.. code-block:: modula2
    PROCEDURE cacos (z: COMPLEX) : COMPLEX ;
.. index::
   cacosl
.. code-block:: modula2
    PROCEDURE cacosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   catanf
.. code-block:: modula2
    PROCEDURE catanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   catan
.. code-block:: modula2
    PROCEDURE catan (z: COMPLEX) : COMPLEX ;
.. index::
   catanl
.. code-block:: modula2
    PROCEDURE catanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   index
.. code-block:: modula2
    PROCEDURE index (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   rindex
.. code-block:: modula2
    PROCEDURE rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   memcmp
.. code-block:: modula2
    PROCEDURE memcmp (s1, s2: ADDRESS; n: CARDINAL) : INTEGER ;
.. index::
   memmove
.. code-block:: modula2
    PROCEDURE memmove (s1, s2: ADDRESS; n: CARDINAL) : ADDRESS ;
.. index::
   memset
.. code-block:: modula2
    PROCEDURE memset (s: ADDRESS; c: INTEGER; n: CARDINAL) : ADDRESS ;
.. index::
   strcat
.. code-block:: modula2
    PROCEDURE strcat (dest, src: ADDRESS) : ADDRESS ;
.. index::
   strncat
.. code-block:: modula2
    PROCEDURE strncat (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
.. index::
   strcpy
.. code-block:: modula2
    PROCEDURE strcpy (dest, src: ADDRESS) : ADDRESS ;
.. index::
   strncpy
.. code-block:: modula2
    PROCEDURE strncpy (dest, src: ADDRESS; n: CARDINAL) : ADDRESS ;
.. index::
   strcmp
.. code-block:: modula2
    PROCEDURE strcmp (s1, s2: ADDRESS) : INTEGER ;
.. index::
   strncmp
.. code-block:: modula2
    PROCEDURE strncmp (s1, s2: ADDRESS; n: CARDINAL) : INTEGER ;
.. index::
   strlen
.. code-block:: modula2
    PROCEDURE strlen (s: ADDRESS) : INTEGER ;
.. index::
   strstr
.. code-block:: modula2
    PROCEDURE strstr (haystack, needle: ADDRESS) : ADDRESS ;
.. index::
   strpbrk
.. code-block:: modula2
    PROCEDURE strpbrk (s, accept: ADDRESS) : ADDRESS ;
.. index::
   strspn
.. code-block:: modula2
    PROCEDURE strspn (s, accept: ADDRESS) : CARDINAL ;
.. index::
   strcspn
.. code-block:: modula2
    PROCEDURE strcspn (s, accept: ADDRESS) : CARDINAL ;
.. index::
   strchr
.. code-block:: modula2
    PROCEDURE strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   strrchr
.. code-block:: modula2
    PROCEDURE strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
    
    END cbuiltin.

@c @node gm2-libs/cgetopt, gm2-libs/cxxabi, gm2-libs/cbuiltin, Base libraries
gm2-libs/cgetopt
----------------

.. code-block:: modula2
    DEFINITION MODULE cgetopt ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    
    TYPE
.. index::
   pair: Options; (type)
.. code-block:: modula2
       Options = ADDRESS ;
    
    VAR
.. index::
   pair: optarg; (var)
.. code-block:: modula2
       optarg                : ADDRESS ;
.. index::
   pair: optind; (var)
   pair: opterr; (var)
   pair: optopt; (var)
.. code-block:: modula2
       optind, opterr, optopt: INTEGER ;
    
    
    (*
       getopt - the getopt() function parses the command-line arguments.
                Its arguments argc and argv are the argument count and array as
                passed to the main() function on program invocation.  An element of
                argv that starts with '-' (and is not exactly "-" or "--") is an
                option element.  The characters of this element (aside from the
                initial '-') are option characters.  If getopt() is called
                repeatedly, it returns successively each of the option characters
                from each of the option elements.
    *)
    
.. index::
   getopt
.. code-block:: modula2
    PROCEDURE getopt (argc: INTEGER; argv: ADDRESS; optstring: ADDRESS) : CHAR ;
    
    
    (*
       getopt_long - works like getopt() except that it also accepts long options,
                     started with two dashes.  (If the program accepts only long
                     options, then optstring should be specified as an empty string (""),
                     not NULL.)  Long option names may be abbreviated if the abbreviation
                     is unique or is an exact match for some defined option.  A
                     long option may take a parameter, of the form --arg=param or
                     --arg param.
    *)
    
.. index::
   getopt_long
.. code-block:: modula2
    PROCEDURE getopt_long (argc: INTEGER; argv: ADDRESS; optstring: ADDRESS;
                           longopts: ADDRESS; VAR longindex: INTEGER) : INTEGER ;
    
    
    (*
       getopt_long_only - a wrapper for the C getopt_long_only.
    *)
    
.. index::
   getopt_long_only
.. code-block:: modula2
    PROCEDURE getopt_long_only (argc: INTEGER; argv: ADDRESS; optstring: ADDRESS;
                                longopts: ADDRESS; VAR longindex: INTEGER) : INTEGER ;
    
    
    (*
       InitOptions - constructor for empty Options.
    *)
    
.. index::
   InitOptions
.. code-block:: modula2
    PROCEDURE InitOptions () : Options ;
    
    
    (*
       KillOptions - deconstructor for empty Options.
    *)
    
.. index::
   KillOptions
.. code-block:: modula2
    PROCEDURE KillOptions (o: Options) : Options ;
    
    
    (*
       SetOption - set option[index] with {name, has_arg, flag, val}.
    *)
    
.. index::
   SetOption
.. code-block:: modula2
    PROCEDURE SetOption (o: Options; index: CARDINAL;
                         name: ADDRESS; has_arg: BOOLEAN;
                         VAR flag: INTEGER; val: INTEGER) ;
    
    
    (*
       GetLongOptionArray - return a pointer to the C array containing all
                            long options.
    *)
    
.. index::
   GetLongOptionArray
.. code-block:: modula2
    PROCEDURE GetLongOptionArray (o: Options) : ADDRESS ;
    
    
    END cgetopt.

@c @node gm2-libs/cxxabi, gm2-libs/dtoa, gm2-libs/cgetopt, Base libraries
gm2-libs/cxxabi
---------------

.. code-block:: modula2
    DEFINITION MODULE FOR "C" cxxabi ;

(* This should only be used by the compiler and it matches the
    g++ implementation.  *)
    
    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT UNQUALIFIED __cxa_begin_catch, __cxa_end_catch, __cxa_rethrow ;
    
    
.. index::
   __cxa_begin_catch
.. code-block:: modula2
    PROCEDURE __cxa_begin_catch (a: ADDRESS) : ADDRESS ;
.. index::
   __cxa_end_catch
.. code-block:: modula2
    PROCEDURE __cxa_end_catch ;
.. index::
   __cxa_rethrow
.. code-block:: modula2
    PROCEDURE __cxa_rethrow ;
    
    
    END cxxabi.

@c @node gm2-libs/dtoa, gm2-libs/errno, gm2-libs/cxxabi, Base libraries
gm2-libs/dtoa
-------------

.. code-block:: modula2
    DEFINITION MODULE dtoa ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    TYPE
.. index::
   pair: Mode; (type)
.. code-block:: modula2
       Mode = (maxsignificant, decimaldigits) ;
    
    
    (*
       strtod - returns a REAL given a string, s.  It will set
                error to TRUE if the number is too large.
    *)
    
.. index::
   strtod
.. code-block:: modula2
    PROCEDURE strtod (s: ADDRESS; VAR error: BOOLEAN) : REAL ;
    
    
    (*
       dtoa - converts a REAL, d, into a string.  The address of the
              string is returned.
              mode       indicates the type of conversion required.
              ndigits    determines the number of digits according to mode.
              decpt      the position of the decimal point.
              sign       does the string have a sign?
    *)
    
.. index::
   dtoa
.. code-block:: modula2
    PROCEDURE dtoa (d        : REAL;
                    mode     : Mode;
                    ndigits  : INTEGER;
    	        VAR decpt: INTEGER;
    	        VAR sign : BOOLEAN) : ADDRESS ;
    
    
    END dtoa.

@c @node gm2-libs/errno, gm2-libs/gdbif, gm2-libs/dtoa, Base libraries
gm2-libs/errno
--------------

.. code-block:: modula2
    DEFINITION MODULE errno ;

    CONST
        EINTR  =  4 ;   (* system call interrupted *)
        ERANGE = 34 ;   (* result is too large     *)
        EAGAIN = 11 ;   (* retry the system call   *)
    
.. index::
   geterrno
.. code-block:: modula2
    PROCEDURE geterrno () : INTEGER ;
    
    
    END errno.

@c @node gm2-libs/gdbif, gm2-libs/ldtoa, gm2-libs/errno, Base libraries
gm2-libs/gdbif
--------------

.. code-block:: modula2
    DEFINITION MODULE gdbif ;

(*  Provides interactive connectivity with gdb useful for debugging
    Modula-2 shared libraries.  *)
    
    EXPORT UNQUALIFIED sleepSpin, finishSpin, connectSpin ;
    
    
    (*
       finishSpin - sets boolean mustWait to FALSE.
    *)
    
.. index::
   finishSpin
.. code-block:: modula2
    PROCEDURE finishSpin ;
    
    
    (*
       sleepSpin - waits for the boolean variable mustWait to become FALSE.
                   It sleeps for a second between each test of the variable.
    *)
    
.. index::
   sleepSpin
.. code-block:: modula2
    PROCEDURE sleepSpin ;
    
    
    (*
       connectSpin - breakpoint placeholder.  Its only purpose is to allow users
                     to set a breakpoint.  This procedure is called once
                     sleepSpin is released from its spin (via a call from
                     finishSpin).
    *)
    
.. index::
   connectSpin
.. code-block:: modula2
    PROCEDURE connectSpin ;
    
    
    END gdbif.

@c @node gm2-libs/ldtoa, gm2-libs/libc, gm2-libs/gdbif, Base libraries
gm2-libs/ldtoa
--------------

.. code-block:: modula2
    DEFINITION MODULE ldtoa ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    TYPE
.. index::
   pair: Mode; (type)
.. code-block:: modula2
       Mode = (maxsignificant, decimaldigits) ;
    
    
    (*
       strtold - returns a LONGREAL given a C string, s.  It will set
                 error to TRUE if the number is too large or badly formed.
    *)
    
.. index::
   strtold
.. code-block:: modula2
    PROCEDURE strtold (s: ADDRESS; VAR error: BOOLEAN) : LONGREAL ;
    
    
    (*
       ldtoa - converts a LONGREAL, d, into a string.  The address of the
               string is returned.
               mode       indicates the type of conversion required.
               ndigits    determines the number of digits according to mode.
               decpt      the position of the decimal point.
               sign       does the string have a sign?
    *)
    
.. index::
   ldtoa
.. code-block:: modula2
    PROCEDURE ldtoa (d        : LONGREAL;
                     mode     : Mode;
                     ndigits  : INTEGER;
                     VAR decpt: INTEGER;
                     VAR sign : BOOLEAN) : ADDRESS ;
    
    
    END ldtoa.

@c @node gm2-libs/libc, gm2-libs/libm, gm2-libs/ldtoa, Base libraries
gm2-libs/libc
-------------

.. code-block:: modula2
    DEFINITION MODULE FOR "C" libc ;

    FROM SYSTEM IMPORT ADDRESS, CSIZE_T, CSSIZE_T ;
    
    EXPORT UNQUALIFIED time_t, timeb, tm, ptrToTM,
                       write, read,
                       system, abort,
                       malloc, free,
                       exit, isatty,
                       getenv, putenv, getpid,
                       dup, close, open, lseek,
                       readv, writev,
                       perror, creat,
                       getcwd, chown, strlen, strcpy, strncpy,
                       unlink, setenv,
                       memcpy, memset, memmove, printf, realloc,
                       rand, srand,
                       time, localtime, ftime,
                       shutdown, rename, setjmp, longjmp, atexit,
                       ttyname, sleep, execv ;
    
    
    TYPE
.. index::
   pair: time_t; (type)
.. code-block:: modula2
       time_t = LONGINT ;
    
.. index::
   pair: ptrToTM; (type)
.. code-block:: modula2
       ptrToTM = POINTER TO tm ;
.. index::
   pair: tm; (type)
.. code-block:: modula2
       tm = RECORD
               tm_sec: INTEGER ;     (* Seconds.     [0-60] (1 leap second) *)
               tm_min: INTEGER ;     (* Minutes.     [0-59]   *)
               tm_hour: INTEGER ;    (* Hours.       [0-23]   *)
               tm_mday: INTEGER ;    (* Day.         [1-31]   *)
               tm_mon: INTEGER ;     (* Month.       [0-11]   *)
               tm_year: INTEGER ;    (* Year - 1900.          *)
               tm_wday: INTEGER ;    (* Day of week. [0-6]    *)
               tm_yday: INTEGER ;    (* Days in year.[0-365]  *)
               tm_isdst: INTEGER ;   (* DST.         [-1/0/1] *)
               tm_gmtoff: LONGINT ;  (* Seconds east of UTC.  *)
               tm_zone: ADDRESS ;    (* char * zone name      *)
.. index::
   pair: END; (type)
.. code-block:: modula2
            END ;
    
.. index::
   pair: timeb; (type)
.. code-block:: modula2
       timeb = RECORD
                  time    : time_t ;
                  millitm : SHORTCARD ;
                  timezone: SHORTCARD ;
                  dstflag : SHORTCARD ;
.. index::
   pair: END; (type)
.. code-block:: modula2
               END ;
    
.. index::
   pair: exitP; (type)
.. code-block:: modula2
       exitP = PROCEDURE () : INTEGER ;
    
    
    (*
         ssize_t write (int d, void *buf, size_t nbytes)
    *)
    
.. index::
   write
.. code-block:: modula2
    PROCEDURE write (d: INTEGER; buf: ADDRESS; nbytes: CSIZE_T) : [ CSSIZE_T ] ;
    
    
    (*
         ssize_t read (int d, void *buf, size_t nbytes)
    *)
    
.. index::
   read
.. code-block:: modula2
    PROCEDURE read (d: INTEGER; buf: ADDRESS; nbytes: CSIZE_T) : [ CSSIZE_T ] ;
    
    
    (*
         int system(string)
         char *string;
    *)
    
.. index::
   system
.. code-block:: modula2
    PROCEDURE system (a: ADDRESS) : [ INTEGER ] ;
    
    
    (*
         abort - generate a fault
    
         abort() first closes all open files if possible, then sends
         an IOT signal to the process.  This signal usually results
         in termination with a core dump, which may be used for
         debugging.
    
         It is possible for abort() to return control if is caught or
         ignored, in which case the value returned is that of the
         kill(2V) system call.
    *)
    
.. index::
   abort
.. code-block:: modula2
    PROCEDURE abort <* noreturn *> ;
    
    
    (*
         malloc - memory allocator.
    
         void *malloc(size_t size);
    
         malloc() returns a pointer to a block of at least size
         bytes, which is appropriately aligned.  If size is zero,
         malloc() returns a non-NULL pointer, but this pointer should
         not be dereferenced.
    *)
    
.. index::
   malloc
.. code-block:: modula2
    PROCEDURE malloc (size: CSIZE_T) : ADDRESS ;
    
    
    (*
         free - memory deallocator.
    
         free (void *ptr);
    
         free() releases a previously allocated block.  Its argument
         is a pointer to a block previously allocated by malloc,
         calloc, realloc, malloc, or memalign.
    *)
    
.. index::
   free
.. code-block:: modula2
    PROCEDURE free (ptr: ADDRESS) ;
    
    
    (*
         void *realloc (void *ptr, size_t size);
    
         realloc changes the size of the memory block pointed to
         by ptr to size bytes. The contents will be  unchanged  to
         the minimum of the old and new sizes; newly allocated memory
         will be uninitialized. If ptr is NIL, the call is
         equivalent  to malloc(size); if size is equal to zero, the
         call is equivalent to free(ptr). Unless ptr is NIL, it
         must have been returned by an earlier call to malloc(),
         realloc.
    *)
    
.. index::
   realloc
.. code-block:: modula2
    PROCEDURE realloc (ptr: ADDRESS; size: CSIZE_T) : ADDRESS ;
    
    
    (*
       isatty - does this descriptor refer to a terminal.
    *)
    
.. index::
   isatty
.. code-block:: modula2
    PROCEDURE isatty (fd: INTEGER) : INTEGER ;
    
    
    (*
       exit - returns control to the invoking process. Result, r, is
              returned.
    *)
    
.. index::
   exit
.. code-block:: modula2
    PROCEDURE exit (r: INTEGER) <* noreturn *> ;
    
    
    (*
       getenv - returns the C string for the equivalent C environment
                variable.
    *)
    
.. index::
   getenv
.. code-block:: modula2
    PROCEDURE getenv (s: ADDRESS) : ADDRESS ;
    
    
    (*
       putenv - change or add an environment variable.
    *)
    
.. index::
   putenv
.. code-block:: modula2
    PROCEDURE putenv (s: ADDRESS) : INTEGER ;
    
    
    (*
       getpid - returns the UNIX process identification number.
    *)
    
.. index::
   getpid
.. code-block:: modula2
    PROCEDURE getpid () : INTEGER ;
    
    
    (*
       dup - duplicates the file descriptor, d.
    *)
    
.. index::
   dup
.. code-block:: modula2
    PROCEDURE dup (d: INTEGER) : INTEGER ;
    
    
    (*
       close - closes the file descriptor, d.
    *)
    
.. index::
   close
.. code-block:: modula2
    PROCEDURE close (d: INTEGER) : [ INTEGER ] ;
    
    
    (*
       open - open the file, filename with flag and mode.
    *)
    
.. index::
   open
.. code-block:: modula2
    PROCEDURE open (filename: ADDRESS; oflag: INTEGER; ...) : INTEGER ;
    
    
    (*
       creat - creates a new file
    *)
    
.. index::
   creat
.. code-block:: modula2
    PROCEDURE creat (filename: ADDRESS; mode: CARDINAL) : INTEGER;
    
    
    (*
       lseek - calls unix lseek:
    
               off_t lseek(int fildes, off_t offset, int whence);
    *)
    
.. index::
   lseek
.. code-block:: modula2
    PROCEDURE lseek (fd: INTEGER; offset: LONGINT; whence: INTEGER) : LONGINT ;
    
    
    (*
       perror - writes errno and string. (ARRAY OF CHAR is translated onto ADDRESS).
    *)
    
.. index::
   perror
.. code-block:: modula2
    PROCEDURE perror (string: ARRAY OF CHAR);
    
    
    (*
       readv - reads an io vector of bytes.
    *)
    
.. index::
   readv
.. code-block:: modula2
    PROCEDURE readv (fd: INTEGER; v: ADDRESS; n: INTEGER) : [ INTEGER ] ;
    
    
    (*
       writev - writes an io vector of bytes.
    *)
    
.. index::
   writev
.. code-block:: modula2
    PROCEDURE writev (fd: INTEGER; v: ADDRESS; n: INTEGER) : [ INTEGER ] ;
    
    
    (*
       getcwd - copies the absolute pathname of the
                current working directory to the array pointed to by buf,
                which is of length size.
    
                If the current absolute path name would require a buffer
                longer than size elements, NULL is returned, and errno is
                set to ERANGE; an application should check for this error,
                and allocate a larger buffer if necessary.
    *)
    
.. index::
   getcwd
.. code-block:: modula2
    PROCEDURE getcwd (buf: ADDRESS; size: CSIZE_T) : ADDRESS ;
    
    
    (*
       chown - The  owner  of  the  file  specified  by  path or by fd is
               changed.  Only the super-user may change the  owner  of  a
               file.   The  owner  of  a file may change the group of the
               file to any group of which that owner is  a  member.   The
               super-user may change the group arbitrarily.
    
               If  the owner or group is specified as -1, then that ID is
               not changed.
    
               On success, zero is returned.  On error, -1  is  returned,
               and errno is set appropriately.
    *)
    
.. index::
   chown
.. code-block:: modula2
    PROCEDURE chown (filename: ADDRESS; uid, gid: INTEGER) : [ INTEGER ] ;
    
    
    (*
       strlen - returns the length of string, a.
    *)
    
.. index::
   strlen
.. code-block:: modula2
    PROCEDURE strlen (a: ADDRESS) : CSIZE_T ;
    
    
    (*
       strcpy - copies string, src, into, dest.
                It returns dest.
    *)
    
.. index::
   strcpy
.. code-block:: modula2
    PROCEDURE strcpy (dest, src: ADDRESS) : [ ADDRESS ] ;
    
    
    (*
       strncpy - copies string, src, into, dest, copying at most, n, bytes.
                 It returns dest.
    *)
    
.. index::
   strncpy
.. code-block:: modula2
    PROCEDURE strncpy (dest, src: ADDRESS; n: CARDINAL) : [ ADDRESS ] ;
    
    
    (*
       unlink - removes file and returns 0 if successful.
    *)
    
.. index::
   unlink
.. code-block:: modula2
    PROCEDURE unlink (file: ADDRESS) : [ INTEGER ] ;
    
    
    (*
       memcpy - copy memory area
    
       SYNOPSIS
    
       #include <string.h>
    
       void *memcpy(void *dest, const void *src, size_t n);
       It returns dest.
    *)
    
.. index::
   memcpy
.. code-block:: modula2
    PROCEDURE memcpy (dest, src: ADDRESS; size: CSIZE_T) : [ ADDRESS ] ;
    
    
    (*
       memset - fill memory with a constant byte
    
       SYNOPSIS
    
       #include <string.h>
    
       void *memset(void *s, int c, size_t n);
       It returns s.
    *)
    
.. index::
   memset
.. code-block:: modula2
    PROCEDURE memset (s: ADDRESS; c: INTEGER; size: CSIZE_T) : [ ADDRESS ] ;
    
    
    (*
       memmove - copy memory areas which may overlap
    
       SYNOPSIS
    
       #include <string.h>
    
       void *memmove(void *dest, const void *src, size_t n);
       It returns dest.
    *)
    
.. index::
   memmove
.. code-block:: modula2
    PROCEDURE memmove (dest, src: ADDRESS; size: CSIZE_T) : [ ADDRESS ] ;
    
    
    (*
       int printf(const char *format, ...);
    *)
    
.. index::
   printf
.. code-block:: modula2
    PROCEDURE printf (format: ARRAY OF CHAR; ...) : [ INTEGER ] ;
    
    
    (*
       setenv - sets environment variable, name, to value.
                It will overwrite an existing value if, overwrite,
                is true.  It returns 0 on success and -1 for an error.
    *)
    
.. index::
   setenv
.. code-block:: modula2
    PROCEDURE setenv (name: ADDRESS; value: ADDRESS; overwrite: INTEGER) : [ INTEGER ] ;
    
    
    (*
       srand - initialize the random number seed.
    *)
    
.. index::
   srand
.. code-block:: modula2
    PROCEDURE srand (seed: INTEGER) ;
    
    
    (*
       rand - return a random integer.
    *)
    
.. index::
   rand
.. code-block:: modula2
    PROCEDURE rand () : INTEGER ;
    
    
    (*
       time - returns a pointer to the time_t value. If, a,
              is not NIL then the libc value is copied into
              memory at address, a.
    *)
    
.. index::
   time
.. code-block:: modula2
    PROCEDURE time (a: ADDRESS) : time_t ;
    
    
    (*
       localtime - returns a pointer to the libc copy of the tm
                   structure.
    *)
    
.. index::
   localtime
.. code-block:: modula2
    PROCEDURE localtime (VAR t: time_t) : ADDRESS ;
    
    
    (*
       ftime - return date and time.
    *)
    
.. index::
   ftime
.. code-block:: modula2
    PROCEDURE ftime (VAR t: timeb) : [ INTEGER ] ;
    
    
    (*
       shutdown - shutdown a socket, s.
                  if how = 0, then no more reads are allowed.
                  if how = 1, then no more writes are allowed.
                  if how = 2, then mo more reads or writes are allowed.
    *)
    
.. index::
   shutdown
.. code-block:: modula2
    PROCEDURE shutdown (s: INTEGER; how: INTEGER) : [ INTEGER ] ;
    
    
    (*
       rename - change the name or location of a file
    *)
    
.. index::
   rename
.. code-block:: modula2
    PROCEDURE rename (oldpath, newpath: ADDRESS) : [ INTEGER ] ;
    
    
    (*
       setjmp - returns 0 if returning directly, and non-zero
                when returning from longjmp using the saved
                context.
    *)
    
.. index::
   setjmp
.. code-block:: modula2
    PROCEDURE setjmp (env: ADDRESS) : INTEGER ;
    
    
    (*
       longjmp - restores the environment saved by the last call
                 of setjmp with the corresponding env argument.
                 After longjmp is completed, program execution
                 continues as if the corresponding call of setjmp
                 had just returned the value val.  The value of
                 val must not be zero.
    *)
    
.. index::
   longjmp
.. code-block:: modula2
    PROCEDURE longjmp (env: ADDRESS; val: INTEGER) ;
    
    
    (*
       atexit - execute, proc, when the function exit is called.
    *)
    
.. index::
   atexit
.. code-block:: modula2
    PROCEDURE atexit (proc: exitP) : [ INTEGER ] ;
    
    
    (*
       ttyname - returns a pointer to a string determining the ttyname.
    *)
    
.. index::
   ttyname
.. code-block:: modula2
    PROCEDURE ttyname (filedes: INTEGER) : ADDRESS ;
    
    
    (*
       sleep - calling thread sleeps for seconds.
    *)
    
.. index::
   sleep
.. code-block:: modula2
    PROCEDURE sleep (seconds: CARDINAL) : [ CARDINAL ] ;
    
    
    (*
       execv - execute a file.
    *)
    
.. index::
   execv
.. code-block:: modula2
    PROCEDURE execv (pathname: ADDRESS; argv: ADDRESS) : [ INTEGER ] ;
    
    
    END libc.

@c @node gm2-libs/libm, gm2-libs/sckt, gm2-libs/libc, Base libraries
gm2-libs/libm
-------------

.. code-block:: modula2
    DEFINITION MODULE FOR "C" libm ;

(* Users are strongly advised to use MathLib0 or RealMath as calls
   to functions within these modules will generate inline code.
   This module is used by MathLib0 and RealMath when inline code cannot
   be generated.  *)
    
    EXPORT UNQUALIFIED sin, sinl, sinf,
                       cos, cosl, cosf,
                       tan, tanl, tanf,
                       sqrt, sqrtl, sqrtf,
                       asin, asinl, asinf,
                       acos, acosl, acosf,
                       atan, atanl, atanf,
                       atan2, atan2l, atan2f,
                       exp, expl, expf,
                       log, logl, logf,
                       exp10, exp10l, exp10f,
                       pow, powl, powf,
                       floor, floorl, floorf,
                       ceil, ceill, ceilf ;
    
.. index::
   sin
.. code-block:: modula2
    PROCEDURE sin (x: REAL) : REAL ;
.. index::
   sinl
.. code-block:: modula2
    PROCEDURE sinl (x: LONGREAL) : LONGREAL ;
.. index::
   sinf
.. code-block:: modula2
    PROCEDURE sinf (x: SHORTREAL) : SHORTREAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE cos (x: REAL) : REAL ;
.. index::
   cosl
.. code-block:: modula2
    PROCEDURE cosl (x: LONGREAL) : LONGREAL ;
.. index::
   cosf
.. code-block:: modula2
    PROCEDURE cosf (x: SHORTREAL) : SHORTREAL ;
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (x: REAL) : REAL ;
.. index::
   tanl
.. code-block:: modula2
    PROCEDURE tanl (x: LONGREAL) : LONGREAL ;
.. index::
   tanf
.. code-block:: modula2
    PROCEDURE tanf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE sqrt (x: REAL) : REAL ;
.. index::
   sqrtl
.. code-block:: modula2
    PROCEDURE sqrtl (x: LONGREAL) : LONGREAL ;
.. index::
   sqrtf
.. code-block:: modula2
    PROCEDURE sqrtf (x: SHORTREAL) : SHORTREAL ;
.. index::
   asin
.. code-block:: modula2
    PROCEDURE asin (x: REAL) : REAL ;
.. index::
   asinl
.. code-block:: modula2
    PROCEDURE asinl (x: LONGREAL) : LONGREAL ;
.. index::
   asinf
.. code-block:: modula2
    PROCEDURE asinf (x: SHORTREAL) : SHORTREAL ;
.. index::
   acos
.. code-block:: modula2
    PROCEDURE acos (x: REAL) : REAL ;
.. index::
   acosl
.. code-block:: modula2
    PROCEDURE acosl (x: LONGREAL) : LONGREAL ;
.. index::
   acosf
.. code-block:: modula2
    PROCEDURE acosf (x: SHORTREAL) : SHORTREAL ;
.. index::
   atan
.. code-block:: modula2
    PROCEDURE atan (x: REAL) : REAL ;
.. index::
   atanl
.. code-block:: modula2
    PROCEDURE atanl (x: LONGREAL) : LONGREAL ;
.. index::
   atanf
.. code-block:: modula2
    PROCEDURE atanf (x: SHORTREAL) : SHORTREAL ;
.. index::
   atan2
.. code-block:: modula2
    PROCEDURE atan2 (x, y: REAL) : REAL ;
.. index::
   atan2l
.. code-block:: modula2
    PROCEDURE atan2l (x, y: LONGREAL) : LONGREAL ;
.. index::
   atan2f
.. code-block:: modula2
    PROCEDURE atan2f (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (x: REAL) : REAL ;
.. index::
   expl
.. code-block:: modula2
    PROCEDURE expl (x: LONGREAL) : LONGREAL ;
.. index::
   expf
.. code-block:: modula2
    PROCEDURE expf (x: SHORTREAL) : SHORTREAL ;
.. index::
   log
.. code-block:: modula2
    PROCEDURE log (x: REAL) : REAL ;
.. index::
   logl
.. code-block:: modula2
    PROCEDURE logl (x: LONGREAL) : LONGREAL ;
.. index::
   logf
.. code-block:: modula2
    PROCEDURE logf (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp10
.. code-block:: modula2
    PROCEDURE exp10 (x: REAL) : REAL ;
.. index::
   exp10l
.. code-block:: modula2
    PROCEDURE exp10l (x: LONGREAL) : LONGREAL ;
.. index::
   exp10f
.. code-block:: modula2
    PROCEDURE exp10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   pow
.. code-block:: modula2
    PROCEDURE pow (x, y: REAL) : REAL ;
.. index::
   powl
.. code-block:: modula2
    PROCEDURE powl (x, y: LONGREAL) : LONGREAL ;
.. index::
   powf
.. code-block:: modula2
    PROCEDURE powf (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   floor
.. code-block:: modula2
    PROCEDURE floor (x: REAL) : REAL ;
.. index::
   floorl
.. code-block:: modula2
    PROCEDURE floorl (x: LONGREAL) : LONGREAL ;
.. index::
   floorf
.. code-block:: modula2
    PROCEDURE floorf (x: SHORTREAL) : SHORTREAL ;
.. index::
   ceil
.. code-block:: modula2
    PROCEDURE ceil (x: REAL) : REAL ;
.. index::
   ceill
.. code-block:: modula2
    PROCEDURE ceill (x: LONGREAL) : LONGREAL ;
.. index::
   ceilf
.. code-block:: modula2
    PROCEDURE ceilf (x: SHORTREAL) : SHORTREAL ;
    
    END libm.

@c @node gm2-libs/sckt, gm2-libs/termios, gm2-libs/libm, Base libraries
gm2-libs/sckt
-------------

.. code-block:: modula2
    DEFINITION MODULE sckt ;

    FROM SYSTEM IMPORT ADDRESS ;
    EXPORT UNQUALIFIED tcpServerState,
                       tcpServerEstablish, tcpServerEstablishPort,
                       tcpServerAccept, getLocalIP,
                       tcpServerPortNo, tcpServerIP, tcpServerSocketFd,
                       tcpServerClientIP, tcpServerClientPortNo,
                       tcpClientState,
                       tcpClientSocket, tcpClientSocketIP, tcpClientConnect,
                       tcpClientPortNo, tcpClientIP, tcpClientSocketFd ;
    
    TYPE
.. index::
   pair: tcpServerState; (type)
.. code-block:: modula2
       tcpServerState = ADDRESS ;
.. index::
   pair: tcpClientState; (type)
.. code-block:: modula2
       tcpClientState = ADDRESS ;
    
    
    (*
       tcpServerEstablish - returns a tcpState containing the relevant
                            information about a socket declared to receive
                            tcp connections.
    *)
    
.. index::
   tcpServerEstablish
.. code-block:: modula2
    PROCEDURE tcpServerEstablish () : tcpServerState ;
    
    
    (*
       tcpServerEstablishPort - returns a tcpState containing the relevant
                                information about a socket declared to receive
                                tcp connections.  This method attempts to use
                                the port specified by the parameter.
    *)
    
.. index::
   tcpServerEstablishPort
.. code-block:: modula2
    PROCEDURE tcpServerEstablishPort (port: CARDINAL) : tcpServerState ;
    
    
    (*
       tcpServerAccept - returns a file descriptor once a client has connected and
                         been accepted.
    *)
    
.. index::
   tcpServerAccept
.. code-block:: modula2
    PROCEDURE tcpServerAccept (s: tcpServerState) : INTEGER ;
    
    
    (*
       tcpServerPortNo - returns the portNo from structure, s.
    *)
    
.. index::
   tcpServerPortNo
.. code-block:: modula2
    PROCEDURE tcpServerPortNo (s: tcpServerState) : CARDINAL ;
    
    
    (*
       tcpSocketFd - returns the sockFd from structure, s.
    *)
    
.. index::
   tcpServerSocketFd
.. code-block:: modula2
    PROCEDURE tcpServerSocketFd (s: tcpServerState) : INTEGER ;
    
    
    (*
       getLocalIP - returns the IP address of this machine.
    *)
    
.. index::
   getLocalIP
.. code-block:: modula2
    PROCEDURE getLocalIP (s: tcpServerState) : CARDINAL ;
    
    
    (*
       tcpServerIP - returns the IP address from structure, s.
    *)
    
.. index::
   tcpServerIP
.. code-block:: modula2
    PROCEDURE tcpServerIP (s: tcpServerState) : CARDINAL ;
    
    
    (*
       tcpServerClientIP - returns the IP address of the client who
                           has connected to server, s.
    *)
    
.. index::
   tcpServerClientIP
.. code-block:: modula2
    PROCEDURE tcpServerClientIP (s: tcpServerState) : CARDINAL ;
    
    
    (*
       tcpServerClientPortNo - returns the port number of the client who
                               has connected to server, s.
    *)
    
.. index::
   tcpServerClientPortNo
.. code-block:: modula2
    PROCEDURE tcpServerClientPortNo (s: tcpServerState) : CARDINAL ;
    
    
    (*
       tcpClientSocket - returns a file descriptor (socket) which has
                         connected to, serverName:portNo.
    *)
    
.. index::
   tcpClientSocket
.. code-block:: modula2
    PROCEDURE tcpClientSocket (serverName: ADDRESS; portNo: CARDINAL) : tcpClientState ;
    
    
    (*
       tcpClientSocketIP - returns a file descriptor (socket) which has
                           connected to, ip:portNo.
    *)
    
.. index::
   tcpClientSocketIP
.. code-block:: modula2
    PROCEDURE tcpClientSocketIP (ip: CARDINAL; portNo: CARDINAL) : tcpClientState ;
    
    
    (*
       tcpClientConnect - returns the file descriptor associated with, s,
                          once a connect has been performed.
    *)
    
.. index::
   tcpClientConnect
.. code-block:: modula2
    PROCEDURE tcpClientConnect (s: tcpClientState) : INTEGER ;
    
    
    (*
       tcpClientPortNo - returns the portNo from structure, s.
    *)
    
.. index::
   tcpClientPortNo
.. code-block:: modula2
    PROCEDURE tcpClientPortNo (s: tcpClientState) : INTEGER ;
    
    
    (*
       tcpClientSocketFd - returns the sockFd from structure, s.
    *)
    
.. index::
   tcpClientSocketFd
.. code-block:: modula2
    PROCEDURE tcpClientSocketFd (s: tcpClientState) : INTEGER ;
    
    
    (*
       tcpClientIP - returns the IP address from structure, s.
    *)
    
.. index::
   tcpClientIP
.. code-block:: modula2
    PROCEDURE tcpClientIP (s: tcpClientState) : CARDINAL ;
    
    
    END sckt.

@c @node gm2-libs/termios, gm2-libs/wrapc, gm2-libs/sckt, Base libraries
gm2-libs/termios
----------------

.. code-block:: modula2
    DEFINITION MODULE termios ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    TYPE
.. index::
   pair: TERMIOS; (type)
.. code-block:: modula2
       TERMIOS = ADDRESS ;
    
.. index::
   pair: ControlChar; (type)
.. code-block:: modula2
       ControlChar = (vintr, vquit, verase, vkill, veof, vtime, vmin,
                      vswtc, vstart, vstop, vsusp, veol, vreprint, vdiscard,
                      vwerase, vlnext, veol2) ;
    
.. index::
   pair: Flag; (type)
.. code-block:: modula2
       Flag = (
               (* input flag bits *)
               ignbrk, ibrkint, ignpar, iparmrk, inpck, istrip, inlcr,
               igncr, icrnl, iuclc, ixon, ixany, ixoff, imaxbel,
               (* output flag bits *)
               opost, olcuc, onlcr, ocrnl, onocr, onlret, ofill, ofdel,
               onl0, onl1, ocr0, ocr1, ocr2, ocr3,
               otab0, otab1, otab2, otab3, obs0, obs1, off0, off1, ovt0, ovt1,
               (* baud rate *)
               b0, b50, b75, b110, b135, b150, b200, b300, b600, b1200,
               b1800, b2400, b4800, b9600, b19200, b38400,
               b57600, b115200, b240400, b460800, b500000, b576000,
               b921600, b1000000, b1152000, b1500000, b2000000, b2500000,
               b3000000, b3500000, b4000000, maxbaud, crtscts,
               (* character size *)
               cs5, cs6, cs7, cs8, cstopb, cread, parenb, parodd, hupcl, clocal,
               (* local flags *)
               lisig, licanon, lxcase, lecho, lechoe, lechok, lechonl, lnoflsh,
               ltopstop, lechoctl, lechoprt, lechoke, lflusho, lpendin, liexten) ;
    
    
    (*
       InitTermios - new data structure.
    *)
    
.. index::
   InitTermios
.. code-block:: modula2
    PROCEDURE InitTermios () : TERMIOS ;
    
    
    (*
       KillTermios - delete data structure.
    *)
    
.. index::
   KillTermios
.. code-block:: modula2
    PROCEDURE KillTermios (t: TERMIOS) : TERMIOS ;
    
    
    (*
       cfgetospeed - return output baud rate.
    *)
    
.. index::
   cfgetospeed
.. code-block:: modula2
    PROCEDURE cfgetospeed (t: TERMIOS) : INTEGER ;
    
    
    (*
       cfgetispeed - return input baud rate.
    *)
    
.. index::
   cfgetispeed
.. code-block:: modula2
    PROCEDURE cfgetispeed (t: TERMIOS) : INTEGER ;
    
    
    (*
       cfsetospeed - set output baud rate.
    *)
    
.. index::
   cfsetospeed
.. code-block:: modula2
    PROCEDURE cfsetospeed (t: TERMIOS; b: CARDINAL) : INTEGER ;
    
    
    (*
       cfsetispeed - set input baud rate.
    *)
    
.. index::
   cfsetispeed
.. code-block:: modula2
    PROCEDURE cfsetispeed (t: TERMIOS; b: CARDINAL) : INTEGER ;
    
    
    (*
       cfsetspeed - set input and output baud rate.
    *)
    
.. index::
   cfsetspeed
.. code-block:: modula2
    PROCEDURE cfsetspeed (t: TERMIOS; b: CARDINAL) : INTEGER ;
    
    
    (*
       tcgetattr - get state of, fd, into, t.
    *)
    
.. index::
   tcgetattr
.. code-block:: modula2
    PROCEDURE tcgetattr (fd: INTEGER; t: TERMIOS) : INTEGER ;
    
    
    (*
       The following three functions return the different option values.
    *)
    
.. index::
   tcsnow
.. code-block:: modula2
    PROCEDURE tcsnow () : INTEGER ;   (* alter fd now *)
.. index::
   tcsdrain
.. code-block:: modula2
    PROCEDURE tcsdrain () : INTEGER ; (* alter when all output has been sent *)
.. index::
   tcsflush
.. code-block:: modula2
    PROCEDURE tcsflush () : INTEGER ; (* like drain, except discard any pending input *)
    
    
    (*
       tcsetattr - set state of, fd, to, t, using option.
    *)
    
.. index::
   tcsetattr
.. code-block:: modula2
    PROCEDURE tcsetattr (fd: INTEGER; option: INTEGER; t: TERMIOS) : INTEGER ;
    
    
    (*
       cfmakeraw - sets, t, to raw mode.
    *)
    
.. index::
   cfmakeraw
.. code-block:: modula2
    PROCEDURE cfmakeraw (t: TERMIOS) ;
    
    
    (*
       tcsendbreak - send zero bits for duration.
    *)
    
.. index::
   tcsendbreak
.. code-block:: modula2
    PROCEDURE tcsendbreak (fd: INTEGER; duration: INTEGER) : INTEGER ;
    
    
    (*
       tcdrain - waits for pending output to be written on, fd.
    *)
    
.. index::
   tcdrain
.. code-block:: modula2
    PROCEDURE tcdrain (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflushi - flush input.
    *)
    
.. index::
   tcflushi
.. code-block:: modula2
    PROCEDURE tcflushi (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflusho - flush output.
    *)
    
.. index::
   tcflusho
.. code-block:: modula2
    PROCEDURE tcflusho (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflushio - flush input and output.
    *)
    
.. index::
   tcflushio
.. code-block:: modula2
    PROCEDURE tcflushio (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflowoni - restart input on, fd.
    *)
    
.. index::
   tcflowoni
.. code-block:: modula2
    PROCEDURE tcflowoni (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflowoffi - stop input on, fd.
    *)
    
.. index::
   tcflowoffi
.. code-block:: modula2
    PROCEDURE tcflowoffi (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflowono - restart output on, fd.
    *)
    
.. index::
   tcflowono
.. code-block:: modula2
    PROCEDURE tcflowono (fd: INTEGER) : INTEGER ;
    
    
    (*
       tcflowoffo - stop output on, fd.
    *)
    
.. index::
   tcflowoffo
.. code-block:: modula2
    PROCEDURE tcflowoffo (fd: INTEGER) : INTEGER ;
    
    
    (*
       GetFlag - sets a flag value from, t, in, b, and returns TRUE
                 if, t, supports, f.
    *)
    
.. index::
   GetFlag
.. code-block:: modula2
    PROCEDURE GetFlag (t: TERMIOS; f: Flag; VAR b: BOOLEAN) : BOOLEAN ;
    
    
    (*
       SetFlag - sets a flag value in, t, to, b, and returns TRUE if
                 this flag value is supported.
    *)
    
.. index::
   SetFlag
.. code-block:: modula2
    PROCEDURE SetFlag (t: TERMIOS; f: Flag; b: BOOLEAN) : BOOLEAN ;
    
    
    (*
       GetChar - sets a CHAR, ch, value from, t, and returns TRUE if
                 this value is supported.
    *)
    
.. index::
   GetChar
.. code-block:: modula2
    PROCEDURE GetChar (t: TERMIOS; c: ControlChar; VAR ch: CHAR) : BOOLEAN ;
    
    
    (*
       SetChar - sets a CHAR value in, t, and returns TRUE if, c,
                 is supported.
    *)
    
.. index::
   SetChar
.. code-block:: modula2
    PROCEDURE SetChar (t: TERMIOS; c: ControlChar; ch: CHAR) : BOOLEAN ;
    
    
    END termios.

@c @node gm2-libs/wrapc, , gm2-libs/termios, Base libraries
gm2-libs/wrapc
--------------

.. code-block:: modula2
    DEFINITION MODULE wrapc ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    EXPORT QUALIFIED strtime, filesize, fileinode,
                     getrand, getusername, filemtime,
                     getnameuidgid, signbit, signbitf, signbitl,
    		 isfinite, isfinitel, isfinitef ;
    
    
    (*
       strtime - returns the C string for the equivalent C asctime
                 function.
    *)
    
.. index::
   strtime
.. code-block:: modula2
    PROCEDURE strtime () : ADDRESS ;
    
    
    (*
       filesize - assigns the size of a file, f, into low, high and
                  returns zero if successful.
    *)
    
.. index::
   filesize
.. code-block:: modula2
    PROCEDURE filesize (f: INTEGER; VAR low, high: CARDINAL) : INTEGER ;
    
    
    (*
       fileinode - return the inode associated with file, f.
    *)
    
.. index::
   fileinode
.. code-block:: modula2
    PROCEDURE fileinode (f: INTEGER; VAR low, high: CARDINAL) : INTEGER ;
    
    
    (*
       filemtime - returns the mtime of a file, f.
    *)
    
.. index::
   filemtime
.. code-block:: modula2
    PROCEDURE filemtime (f: INTEGER) : INTEGER ;
    
    
    (*
       getrand - returns a random number between 0..n-1
    *)
    
.. index::
   getrand
.. code-block:: modula2
    PROCEDURE getrand (n: INTEGER) : INTEGER ;
    
    
    (*
       getusername - returns a C string describing the current user.
    *)
    
.. index::
   getusername
.. code-block:: modula2
    PROCEDURE getusername () : ADDRESS ;
    
    
    (*
       getnameuidgid - fills in the, uid, and, gid, which represents
                       user, name.
    *)
    
.. index::
   getnameuidgid
.. code-block:: modula2
    PROCEDURE getnameuidgid (name: ADDRESS; VAR uid, gid: INTEGER) ;
    
    
    (*
       in C these procedure functions are really macros, so we provide
       real C functions and let gm2 call these if the builtins
       are unavailable.
    *)
    
.. index::
   signbit
.. code-block:: modula2
    PROCEDURE signbit (r: REAL) : INTEGER ;
.. index::
   signbitf
.. code-block:: modula2
    PROCEDURE signbitf (s: SHORTREAL) : INTEGER ;
.. index::
   signbitl
.. code-block:: modula2
    PROCEDURE signbitl (l: LONGREAL) : INTEGER ;
    
    
    (*
       isfinite - provide non builtin alternative to the gcc builtin isfinite.
                  Returns 1 if x is finite and 0 if it is not.
    *)
    
.. index::
   isfinite
.. code-block:: modula2
    PROCEDURE isfinite (x: REAL) : INTEGER ;
    
    
    (*
       isfinitef - provide non builtin alternative to the gcc builtin isfinite.
                   Returns 1 if x is finite and 0 if it is not.
    *)
    
.. index::
   isfinitef
.. code-block:: modula2
    PROCEDURE isfinitef (x: SHORTREAL) : INTEGER ;
    
    
    (*
       isfinitel - provide non builtin alternative to the gcc builtin isfinite.
                   Returns 1 if x is finite and 0 if it is not.
    *)
    
.. index::
   isfinitel
.. code-block:: modula2
    PROCEDURE isfinitel (x: LONGREAL) : INTEGER ;
    
    
    END wrapc.


@c ------------------------------------------------------------
@c @node PIM and Logitech 3.0 Compatible, PIM coroutine support, Base libraries, Libraries
PIM and Logitech 3.0 Compatible
===============================

@menu
* gm2-libs-pim/BitBlockOps::BitBlockOps.def
* gm2-libs-pim/BitByteOps::BitByteOps.def
* gm2-libs-pim/BitWordOps::BitWordOps.def
* gm2-libs-pim/BlockOps::BlockOps.def
* gm2-libs-pim/Break::Break.def
* gm2-libs-pim/CardinalIO::CardinalIO.def
* gm2-libs-pim/Conversions::Conversions.def
* gm2-libs-pim/DebugPMD::DebugPMD.def
* gm2-libs-pim/DebugTrace::DebugTrace.def
* gm2-libs-pim/Delay::Delay.def
* gm2-libs-pim/Display::Display.def
* gm2-libs-pim/ErrorCode::ErrorCode.def
* gm2-libs-pim/FileSystem::FileSystem.def
* gm2-libs-pim/FloatingUtilities::FloatingUtilities.def
* gm2-libs-pim/InOut::InOut.def
* gm2-libs-pim/Keyboard::Keyboard.def
* gm2-libs-pim/LongIO::LongIO.def
* gm2-libs-pim/NumberConversion::NumberConversion.def
* gm2-libs-pim/Random::Random.def
* gm2-libs-pim/RealConversions::RealConversions.def
* gm2-libs-pim/RealInOut::RealInOut.def
* gm2-libs-pim/Strings::Strings.def
* gm2-libs-pim/Termbase::Termbase.def
* gm2-libs-pim/Terminal::Terminal.def
* gm2-libs-pim/TimeDate::TimeDate.def
@end menu

@c @node gm2-libs-pim/BitBlockOps, gm2-libs-pim/BitByteOps, , PIM and Logitech 3.0 Compatible
gm2-libs-pim/BitBlockOps
------------------------

.. code-block:: modula2
    DEFINITION MODULE BitBlockOps ;

    
    FROM SYSTEM IMPORT ADDRESS ;
    
    
    (*
       BlockAnd - performs a bitwise AND on blocks
                  [dest..dest+size-1] := [dest..dest+size-1] AND
                                         [src..src+size-1]
    *)
    
.. index::
   BlockAnd
.. code-block:: modula2
    PROCEDURE BlockAnd (dest, src: ADDRESS; size: CARDINAL) ;
    
    
    (*
       BlockOr - performs a bitwise OR on blocks
                 [dest..dest+size-1] := [dest..dest+size-1] OR
                                        [src..src+size-1]
    *)
    
.. index::
   BlockOr
.. code-block:: modula2
    PROCEDURE BlockOr (dest, src: ADDRESS; size: CARDINAL) ;
    
    
    (*
       BlockXor - performs a bitwise XOR on blocks
                  [dest..dest+size-1] := [dest..dest+size-1] XOR
                                         [src..src+size-1]
    *)
    
.. index::
   BlockXor
.. code-block:: modula2
    PROCEDURE BlockXor (dest, src: ADDRESS; size: CARDINAL) ;
    
    
    (*
       BlockNot - performs a bitsize NOT on the block as defined
                  by:  [dest..dest+size-1]
    *)
    
.. index::
   BlockNot
.. code-block:: modula2
    PROCEDURE BlockNot (dest: ADDRESS; size: CARDINAL) ;
    
    
    (*
       BlockShr - performs a block shift right of, count, bits.
                  Where the block is defined as:
                  [dest..dest+size-1].
                  The block is considered to be an ARRAY OF BYTEs
                  which is shifted, bit at a time over each byte in
                  turn.  The left most byte is considered the byte
                  located at the lowest address.
                  If you require an endianness SHIFT use
                  the SYSTEM.SHIFT procedure and declare the
                  block as a POINTER TO set type.
    *)
    
.. index::
   BlockShr
.. code-block:: modula2
    PROCEDURE BlockShr (dest: ADDRESS; size, count: CARDINAL) ;
    
    
    (*
       BlockShl - performs a block shift left of, count, bits.
                  Where the block is defined as:
                  [dest..dest+size-1].
                  The block is considered to be an ARRAY OF BYTEs
                  which is shifted, bit at a time over each byte in
                  turn.  The left most byte is considered the byte
                  located at the lowest address.
                  If you require an endianness SHIFT use
                  the SYSTEM.SHIFT procedure and declare the
                  block as a POINTER TO set type.
    *)
    
.. index::
   BlockShl
.. code-block:: modula2
    PROCEDURE BlockShl (dest: ADDRESS; size, count: CARDINAL) ;
    
    
    (*
       BlockRor - performs a block rotate right of, count, bits.
                  Where the block is defined as:
                  [dest..dest+size-1].
                  The block is considered to be an ARRAY OF BYTEs
                  which is rotated, bit at a time over each byte in
                  turn.  The left most byte is considered the byte
                  located at the lowest address.
                  If you require an endianness ROTATE use
                  the SYSTEM.ROTATE procedure and declare the
                  block as a POINTER TO set type.
    *)
    
.. index::
   BlockRor
.. code-block:: modula2
    PROCEDURE BlockRor (dest: ADDRESS; size, count: CARDINAL) ;
    
    
    (*
       BlockRol - performs a block rotate left of, count, bits.
                  Where the block is defined as:
                  [dest..dest+size-1].
                  The block is considered to be an ARRAY OF BYTEs
                  which is rotated, bit at a time over each byte in
                  turn.  The left most byte is considered the byte
                  located at the lowest address.
                  If you require an endianness ROTATE use
                  the SYSTEM.ROTATE procedure and declare the
                  block as a POINTER TO set type.
    *)
    
.. index::
   BlockRol
.. code-block:: modula2
    PROCEDURE BlockRol (dest: ADDRESS; size, count: CARDINAL) ;
    
    
    END BitBlockOps.

@c @node gm2-libs-pim/BitByteOps, gm2-libs-pim/BitWordOps, gm2-libs-pim/BitBlockOps, PIM and Logitech 3.0 Compatible
gm2-libs-pim/BitByteOps
-----------------------

.. code-block:: modula2
    DEFINITION MODULE BitByteOps ;

    FROM SYSTEM IMPORT BYTE ;
    
    
    (*
       GetBits - returns the bits firstBit..lastBit from source.
                 Bit 0 of byte maps onto the firstBit of source.
    *)
    
.. index::
   GetBits
.. code-block:: modula2
    PROCEDURE GetBits (source: BYTE; firstBit, lastBit: CARDINAL) : BYTE ;
    
    
    (*
       SetBits - sets bits in, byte, starting at, firstBit, and ending at,
                 lastBit, with, pattern.  The bit zero of, pattern, will
                 be placed into, byte, at position, firstBit.
    *)
    
.. index::
   SetBits
.. code-block:: modula2
    PROCEDURE SetBits (VAR byte: BYTE; firstBit, lastBit: CARDINAL;
                       pattern: BYTE) ;
    
    
    (*
       ByteAnd - returns a bitwise (left AND right)
    *)
    
.. index::
   ByteAnd
.. code-block:: modula2
    PROCEDURE ByteAnd (left, right: BYTE) : BYTE ;
    
    
    (*
       ByteOr - returns a bitwise (left OR right)
    *)
    
.. index::
   ByteOr
.. code-block:: modula2
    PROCEDURE ByteOr (left, right: BYTE) : BYTE ;
    
    
    (*
       ByteXor - returns a bitwise (left XOR right)
    *)
    
.. index::
   ByteXor
.. code-block:: modula2
    PROCEDURE ByteXor (left, right: BYTE) : BYTE ;
    
    
    (*
       ByteNot - returns a byte with all bits inverted.
    *)
    
.. index::
   ByteNot
.. code-block:: modula2
    PROCEDURE ByteNot (byte: BYTE) : BYTE ;
    
    
    (*
       ByteShr - returns a, byte, which has been shifted, count
                 bits to the right.
    *)
    
.. index::
   ByteShr
.. code-block:: modula2
    PROCEDURE ByteShr (byte: BYTE; count: CARDINAL) : BYTE ;
    
    
    (*
       ByteShl - returns a, byte, which has been shifted, count
                 bits to the left.
    *)
    
.. index::
   ByteShl
.. code-block:: modula2
    PROCEDURE ByteShl (byte: BYTE; count: CARDINAL) : BYTE ;
    
    
    (*
       ByteSar - shift byte arthemetic right.  Preserves the top
                 end bit and as the value is shifted right.
    *)
    
.. index::
   ByteSar
.. code-block:: modula2
    PROCEDURE ByteSar (byte: BYTE; count: CARDINAL) : BYTE ;
    
    
    (*
       ByteRor - returns a, byte, which has been rotated, count
                 bits to the right.
    *)
    
.. index::
   ByteRor
.. code-block:: modula2
    PROCEDURE ByteRor (byte: BYTE; count: CARDINAL) : BYTE ;
    
    
    (*
       ByteRol - returns a, byte, which has been rotated, count
                 bits to the left.
    *)
    
.. index::
   ByteRol
.. code-block:: modula2
    PROCEDURE ByteRol (byte: BYTE; count: CARDINAL) : BYTE ;
    
    
    (*
       HighNibble - returns the top nibble only from, byte.
                    The top nibble of, byte, is extracted and
                    returned in the bottom nibble of the return
                    value.
    *)
    
.. index::
   HighNibble
.. code-block:: modula2
    PROCEDURE HighNibble (byte: BYTE) : BYTE ;
    
    
    (*
       LowNibble - returns the low nibble only from, byte.
                   The top nibble is replaced by zeros.
    *)
    
.. index::
   LowNibble
.. code-block:: modula2
    PROCEDURE LowNibble (byte: BYTE) : BYTE ;
    
    
    (*
       Swap - swaps the low and high nibbles in the, byte.
    *)
    
.. index::
   Swap
.. code-block:: modula2
    PROCEDURE Swap (byte: BYTE) : BYTE ;
    
    
    END BitByteOps.

@c @node gm2-libs-pim/BitWordOps, gm2-libs-pim/BlockOps, gm2-libs-pim/BitByteOps, PIM and Logitech 3.0 Compatible
gm2-libs-pim/BitWordOps
-----------------------

.. code-block:: modula2
    DEFINITION MODULE BitWordOps ;

    FROM SYSTEM IMPORT WORD ;
    
    
    (*
       GetBits - returns the bits firstBit..lastBit from source.
                 Bit 0 of word maps onto the firstBit of source.
    *)
    
.. index::
   GetBits
.. code-block:: modula2
    PROCEDURE GetBits (source: WORD; firstBit, lastBit: CARDINAL) : WORD ;
    
    
    (*
       SetBits - sets bits in, word, starting at, firstBit, and ending at,
                 lastBit, with, pattern.  The bit zero of, pattern, will
                 be placed into, word, at position, firstBit.
    *)
    
.. index::
   SetBits
.. code-block:: modula2
    PROCEDURE SetBits (VAR word: WORD; firstBit, lastBit: CARDINAL;
                       pattern: WORD) ;
    
    
    (*
       WordAnd - returns a bitwise (left AND right)
    *)
    
.. index::
   WordAnd
.. code-block:: modula2
    PROCEDURE WordAnd (left, right: WORD) : WORD ;
    
    
    (*
       WordOr - returns a bitwise (left OR right)
    *)
    
.. index::
   WordOr
.. code-block:: modula2
    PROCEDURE WordOr (left, right: WORD) : WORD ;
    
    
    (*
       WordXor - returns a bitwise (left XOR right)
    *)
    
.. index::
   WordXor
.. code-block:: modula2
    PROCEDURE WordXor (left, right: WORD) : WORD ;
    
    
    (*
       WordNot - returns a word with all bits inverted.
    *)
    
.. index::
   WordNot
.. code-block:: modula2
    PROCEDURE WordNot (word: WORD) : WORD ;
    
    
    (*
       WordShr - returns a, word, which has been shifted, count
                 bits to the right.
    *)
    
.. index::
   WordShr
.. code-block:: modula2
    PROCEDURE WordShr (word: WORD; count: CARDINAL) : WORD ;
    
    
    (*
       WordShl - returns a, word, which has been shifted, count
                 bits to the left.
    *)
    
.. index::
   WordShl
.. code-block:: modula2
    PROCEDURE WordShl (word: WORD; count: CARDINAL) : WORD ;
    
    
    (*
       WordSar - shift word arthemetic right.  Preserves the top
                 end bit and as the value is shifted right.
    *)
    
.. index::
   WordSar
.. code-block:: modula2
    PROCEDURE WordSar (word: WORD; count: CARDINAL) : WORD ;
    
    
    (*
       WordRor - returns a, word, which has been rotated, count
                 bits to the right.
    *)
    
.. index::
   WordRor
.. code-block:: modula2
    PROCEDURE WordRor (word: WORD; count: CARDINAL) : WORD ;
    
    
    (*
       WordRol - returns a, word, which has been rotated, count
                 bits to the left.
    *)
    
.. index::
   WordRol
.. code-block:: modula2
    PROCEDURE WordRol (word: WORD; count: CARDINAL) : WORD ;
    
    
    (*
       HighByte - returns the top byte only from, word.
                  The byte is returned in the bottom byte
                  in the return value.
    *)
    
.. index::
   HighByte
.. code-block:: modula2
    PROCEDURE HighByte (word: WORD) : WORD ;
    
    
    (*
       LowByte - returns the low byte only from, word.
                 The byte is returned in the bottom byte
                 in the return value.
    *)
    
.. index::
   LowByte
.. code-block:: modula2
    PROCEDURE LowByte (word: WORD) : WORD ;
    
    
    (*
       Swap - byte flips the contents of word.
    *)
    
.. index::
   Swap
.. code-block:: modula2
    PROCEDURE Swap (word: WORD) : WORD ;
    
    
    END BitWordOps.

@c @node gm2-libs-pim/BlockOps, gm2-libs-pim/Break, gm2-libs-pim/BitWordOps, PIM and Logitech 3.0 Compatible
gm2-libs-pim/BlockOps
---------------------

.. code-block:: modula2
    DEFINITION MODULE BlockOps ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    
    (*
       MoveBlockForward - moves, n, bytes from, src, to, dest.
                          Starts copying from src and keep copying
                          until, n, bytes have been copied.
    *)
    
.. index::
   BlockMoveForward
.. code-block:: modula2
    PROCEDURE BlockMoveForward (dest, src: ADDRESS; n: CARDINAL) ;
    
    
    (*
       MoveBlockBackward - moves, n, bytes from, src, to, dest.
                           Starts copying from src+n and keeps copying
                           until, n, bytes have been copied.
                           The last datum to be copied will be the byte
                           at address, src.
    *)
    
.. index::
   BlockMoveBackward
.. code-block:: modula2
    PROCEDURE BlockMoveBackward (dest, src: ADDRESS; n: CARDINAL) ;
    
    
    (*
       BlockClear - fills, block..block+n-1, with zero's.
    *)
    
.. index::
   BlockClear
.. code-block:: modula2
    PROCEDURE BlockClear (block: ADDRESS; n: CARDINAL) ;
    
    
    (*
       BlockSet - fills, n, bytes starting at, block, with a pattern
                  defined at address pattern..pattern+patternSize-1.
    *)
    
.. index::
   BlockSet
.. code-block:: modula2
    PROCEDURE BlockSet (block: ADDRESS; n: CARDINAL;
                        pattern: ADDRESS; patternSize: CARDINAL) ;
    
    
    (*
       BlockEqual - returns TRUE if the blocks defined, a..a+n-1, and,
                    b..b+n-1 contain the same bytes.
    *)
    
.. index::
   BlockEqual
.. code-block:: modula2
    PROCEDURE BlockEqual (a, b: ADDRESS; n: CARDINAL) : BOOLEAN ;
    
    
    (*
       BlockPosition - searches for a pattern as defined by
                       pattern..patternSize-1 in the block,
                       block..block+blockSize-1.  It returns
                       the offset from block indicating the
                       first occurence of, pattern.
                       MAX(CARDINAL) is returned if no match
                       is detected.
    *)
    
.. index::
   BlockPosition
.. code-block:: modula2
    PROCEDURE BlockPosition (block: ADDRESS; blockSize: CARDINAL;
                             pattern: ADDRESS; patternSize: CARDINAL) : CARDINAL ;
    
    
    END BlockOps.

@c @node gm2-libs-pim/Break, gm2-libs-pim/CardinalIO, gm2-libs-pim/BlockOps, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Break
------------------

.. code-block:: modula2
    DEFINITION MODULE Break ;

    
    EXPORT QUALIFIED EnableBreak, DisableBreak, InstallBreak, UnInstallBreak ;
    
    
    (*
       EnableBreak - enable the current break handler.
    *)
    
.. index::
   EnableBreak
.. code-block:: modula2
    PROCEDURE EnableBreak ;
    
    
    (*
       DisableBreak - disable the current break handler (and all
                      installed handlers).
    *)
    
.. index::
   DisableBreak
.. code-block:: modula2
    PROCEDURE DisableBreak ;
    
    
    (*
       InstallBreak - installs a procedure, p, to be invoked when
                      a ctrl-c is caught. Any number of these
                      procedures may be stacked. Only the top
                      procedure is run when ctrl-c is caught.
    *)
    
.. index::
   InstallBreak
.. code-block:: modula2
    PROCEDURE InstallBreak (p: PROC) ;
    
    
    (*
       UnInstallBreak - pops the break handler stack.
    *)
    
.. index::
   UnInstallBreak
.. code-block:: modula2
    PROCEDURE UnInstallBreak ;
    
    
    END Break.

@c @node gm2-libs-pim/CardinalIO, gm2-libs-pim/Conversions, gm2-libs-pim/Break, PIM and Logitech 3.0 Compatible
gm2-libs-pim/CardinalIO
-----------------------

.. code-block:: modula2
    DEFINITION MODULE CardinalIO ;

    EXPORT QUALIFIED Done,
                     ReadCardinal, WriteCardinal, ReadHex, WriteHex,
                     ReadLongCardinal, WriteLongCardinal, ReadLongHex,
                     WriteLongHex,
                     ReadShortCardinal, WriteShortCardinal, ReadShortHex,
                     WriteShortHex ;
    
    
    VAR
.. index::
   pair: Done; (var)
.. code-block:: modula2
       Done: BOOLEAN ;
    
    
    (*
       ReadCardinal - read an unsigned decimal number from the terminal.
                      The read continues until a space, newline, esc or
                      end of file is reached.
    *)
    
.. index::
   ReadCardinal
.. code-block:: modula2
    PROCEDURE ReadCardinal (VAR c: CARDINAL) ;
    
    
    (*
       WriteCardinal - writes the value, c, to the terminal and ensures
                       that at least, n, characters are written. The number
                       will be padded out by preceeding spaces if necessary.
    *)
    
.. index::
   WriteCardinal
.. code-block:: modula2
    PROCEDURE WriteCardinal (c: CARDINAL; n: CARDINAL) ;
    
    
    (*
       ReadHex - reads in an unsigned hexadecimal number from the terminal.
                 The read continues until a space, newline, esc or
                 end of file is reached.
    *)
    
.. index::
   ReadHex
.. code-block:: modula2
    PROCEDURE ReadHex (VAR c: CARDINAL) ;
    
    
    (*
       WriteHex - writes out a CARDINAL, c, in hexadecimal format padding
                  with, n, characters (leading with '0')
    *)
    
.. index::
   WriteHex
.. code-block:: modula2
    PROCEDURE WriteHex (c: CARDINAL; n: CARDINAL) ;
    
    
    (*
       ReadLongCardinal - read an unsigned decimal number from the terminal.
                          The read continues until a space, newline, esc or
                          end of file is reached.
    *)
    
.. index::
   ReadLongCardinal
.. code-block:: modula2
    PROCEDURE ReadLongCardinal (VAR c: LONGCARD) ;
    
    
    (*
       WriteLongCardinal - writes the value, c, to the terminal and ensures
                           that at least, n, characters are written. The number
                           will be padded out by preceeding spaces if necessary.
    *)
    
.. index::
   WriteLongCardinal
.. code-block:: modula2
    PROCEDURE WriteLongCardinal (c: LONGCARD; n: CARDINAL) ;
    
    
    (*
       ReadLongHex - reads in an unsigned hexadecimal number from the terminal.
                     The read continues until a space, newline, esc or
                     end of file is reached.
    *)
    
.. index::
   ReadLongHex
.. code-block:: modula2
    PROCEDURE ReadLongHex (VAR c: LONGCARD) ;
    
    
    (*
       WriteLongHex - writes out a LONGCARD, c, in hexadecimal format padding
                      with, n, characters (leading with '0')
    *)
    
.. index::
   WriteLongHex
.. code-block:: modula2
    PROCEDURE WriteLongHex (c: LONGCARD; n: CARDINAL) ;
    
    
    (*
       WriteShortCardinal - writes the value, c, to the terminal and ensures
                           that at least, n, characters are written. The number
                           will be padded out by preceeding spaces if necessary.
    *)
    
.. index::
   WriteShortCardinal
.. code-block:: modula2
    PROCEDURE WriteShortCardinal (c: SHORTCARD; n: CARDINAL) ;
    
    
    (*
       ReadShortCardinal - read an unsigned decimal number from the terminal.
                           The read continues until a space, newline, esc or
                           end of file is reached.
    *)
    
.. index::
   ReadShortCardinal
.. code-block:: modula2
    PROCEDURE ReadShortCardinal (VAR c: SHORTCARD) ;
    
    
    (*
       ReadShortHex - reads in an unsigned hexadecimal number from the terminal.
                     The read continues until a space, newline, esc or
                     end of file is reached.
    *)
    
.. index::
   ReadShortHex
.. code-block:: modula2
    PROCEDURE ReadShortHex (VAR c: SHORTCARD) ;
    
    
    (*
       WriteShortHex - writes out a SHORTCARD, c, in hexadecimal format padding
                      with, n, characters (leading with '0')
    *)
    
.. index::
   WriteShortHex
.. code-block:: modula2
    PROCEDURE WriteShortHex (c: SHORTCARD; n: CARDINAL) ;
    
    
    END CardinalIO.

@c @node gm2-libs-pim/Conversions, gm2-libs-pim/DebugPMD, gm2-libs-pim/CardinalIO, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Conversions
------------------------

.. code-block:: modula2
    DEFINITION MODULE Conversions ;

    EXPORT QUALIFIED ConvertOctal, ConvertHex, ConvertCardinal,
                     ConvertInteger, ConvertLongInt, ConvertShortInt ;
    
    (*
       ConvertOctal - converts a CARDINAL, num, into an octal/hex/decimal
                      string and right justifies the string. It adds
                      spaces rather than '0' to pad out the string
                      to len characters.
    
                      If the length of str is < num then the number is
                      truncated on the right.
    *)
    
.. index::
   ConvertOctal
.. code-block:: modula2
    PROCEDURE ConvertOctal    (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
.. index::
   ConvertHex
.. code-block:: modula2
    PROCEDURE ConvertHex      (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
.. index::
   ConvertCardinal
.. code-block:: modula2
    PROCEDURE ConvertCardinal (num, len: CARDINAL; VAR str: ARRAY OF CHAR) ;
    
    (*
       The INTEGER counterparts will add a '-' if, num, is <0
    *)
    
.. index::
   ConvertInteger
.. code-block:: modula2
    PROCEDURE ConvertInteger  (num: INTEGER; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
.. index::
   ConvertLongInt
.. code-block:: modula2
    PROCEDURE ConvertLongInt  (num: LONGINT; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
.. index::
   ConvertShortInt
.. code-block:: modula2
    PROCEDURE ConvertShortInt (num: SHORTINT; len: CARDINAL; VAR str: ARRAY OF CHAR) ;
    
    
    END Conversions.

@c @node gm2-libs-pim/DebugPMD, gm2-libs-pim/DebugTrace, gm2-libs-pim/Conversions, PIM and Logitech 3.0 Compatible
gm2-libs-pim/DebugPMD
---------------------

.. code-block:: modula2
    DEFINITION MODULE DebugPMD ;

    END DebugPMD.

@c @node gm2-libs-pim/DebugTrace, gm2-libs-pim/Delay, gm2-libs-pim/DebugPMD, PIM and Logitech 3.0 Compatible
gm2-libs-pim/DebugTrace
-----------------------

.. code-block:: modula2
    DEFINITION MODULE DebugTrace ;

    END DebugTrace.

@c @node gm2-libs-pim/Delay, gm2-libs-pim/Display, gm2-libs-pim/DebugTrace, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Delay
------------------

.. code-block:: modula2
    DEFINITION MODULE Delay ;

    EXPORT QUALIFIED Delay ;
    
    
    (*
       milliSec - delays the program by approximately, milliSec, milliseconds.
    *)
    
.. index::
   Delay
.. code-block:: modula2
    PROCEDURE Delay (milliSec: INTEGER) ;
    
    
    END Delay.

@c @node gm2-libs-pim/Display, gm2-libs-pim/ErrorCode, gm2-libs-pim/Delay, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Display
--------------------

.. code-block:: modula2
    DEFINITION MODULE Display ;

    EXPORT QUALIFIED Write ;
    
    
    (*
       Write - display a character to the stdout.
               ASCII.EOL moves to the beginning of the next line.
               ASCII.del erases the character to the left of the cursor.
    *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (ch: CHAR) ;
    
    
    END Display.

@c @node gm2-libs-pim/ErrorCode, gm2-libs-pim/FileSystem, gm2-libs-pim/Display, PIM and Logitech 3.0 Compatible
gm2-libs-pim/ErrorCode
----------------------

.. code-block:: modula2
    DEFINITION MODULE ErrorCode ;

    EXPORT QUALIFIED SetErrorCode, GetErrorCode, ExitToOS ;
    
    
    (*
       SetErrorCode - sets the exit value which will be used if
                      the application terminates normally.
    *)
    
.. index::
   SetErrorCode
.. code-block:: modula2
    PROCEDURE SetErrorCode (value: INTEGER) ;
    
    
    (*
       GetErrorCode - returns the current value to be used upon
                      application termination.
    *)
    
.. index::
   GetErrorCode
.. code-block:: modula2
    PROCEDURE GetErrorCode (VAR value: INTEGER) ;
    
    
    (*
       ExitToOS - terminate the application and exit returning
                  the last value set by SetErrorCode to the OS.
    *)
    
.. index::
   ExitToOS
.. code-block:: modula2
    PROCEDURE ExitToOS ;
    
    
    END ErrorCode.

@c @node gm2-libs-pim/FileSystem, gm2-libs-pim/FloatingUtilities, gm2-libs-pim/ErrorCode, PIM and Logitech 3.0 Compatible
gm2-libs-pim/FileSystem
-----------------------

.. code-block:: modula2
    DEFINITION MODULE FileSystem ;

(*  Use this module sparingly, FIO or the ISO file modules have a
    much cleaner interface.  *)
    
    FROM SYSTEM IMPORT WORD, BYTE, ADDRESS ;
    IMPORT FIO ;
    FROM DynamicStrings IMPORT String ;
    
    EXPORT QUALIFIED File, Response, Flag, FlagSet,
    
                     Create, Close, Lookup, Rename, Delete,
                     SetRead, SetWrite, SetModify, SetOpen,
                     Doio, SetPos, GetPos, Length, Reset,
    
                     ReadWord, ReadChar, ReadByte, ReadNBytes,
                     WriteWord, WriteChar, WriteByte, WriteNBytes ;
    
    TYPE
.. index::
   pair: File; (type)
.. code-block:: modula2
       File = RECORD
                 res     : Response ;
                 flags   : FlagSet ;
                 eof     : BOOLEAN ;
                 lastWord: WORD ;
                 lastByte: BYTE ;
                 fio     : FIO.File ;
                 highpos,
                 lowpos  : CARDINAL ;
                 name    : String ;
.. index::
   pair: END; (type)
.. code-block:: modula2
              END ;
    
.. index::
   pair: Flag; (type)
.. code-block:: modula2
       Flag = (
               read,        (* read access mode *)
               write,       (* write access mode *)
               modify,
               truncate,    (* truncate file when closed *)
               again,       (* reread the last character *)
               temporary,   (* file is temporary *)
               opened       (* file has been opened *)
              );
    
.. index::
   pair: FlagSet; (type)
.. code-block:: modula2
       FlagSet = SET OF Flag;
    
.. index::
   pair: Response; (type)
.. code-block:: modula2
       Response = (done, notdone, notsupported, callerror,
                   unknownfile, paramerror, toomanyfiles,
.. index::
   pair: userdeverror); (type)
.. code-block:: modula2
                   userdeverror) ;
    
.. index::
   pair: Command; (type)
.. code-block:: modula2
       Command = (create, close, lookup, rename, delete,
                  setread, setwrite, setmodify, setopen,
                  doio, setpos, getpos, length) ;
    
    
    (*
       Create - creates a temporary file. To make the file perminant
                the file must be renamed.
    *)
    
.. index::
   Create
.. code-block:: modula2
    PROCEDURE Create (VAR f: File) ;
    
    
    (*
       Close - closes an open file.
    *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (f: File) ;
    
    
    (*
       Lookup - looks for a file, filename. If the file is found
                then, f, is opened. If it is not found and, newFile,
                is TRUE then a new file is created and attached to, f.
                If, newFile, is FALSE and no file was found then f.res
                is set to notdone.
    *)
    
.. index::
   Lookup
.. code-block:: modula2
    PROCEDURE Lookup (VAR f: File; filename: ARRAY OF CHAR; newFile: BOOLEAN) ;
    
    
    (*
       Rename - rename a file and change a temporary file to a permanent
                file. f.res is set appropriately.
    *)
    
.. index::
   Rename
.. code-block:: modula2
    PROCEDURE Rename (VAR f: File; newname: ARRAY OF CHAR) ;
    
    
    (*
       Delete - deletes a file, name, and sets the f.res field.
                f.res is set appropriately.
    *)
    
.. index::
   Delete
.. code-block:: modula2
    PROCEDURE Delete (name: ARRAY OF CHAR; VAR f: File) ;
    
    
    (*
       ReadWord - reads a WORD, w, from file, f.
                  f.res is set appropriately.
    *)
    
.. index::
   ReadWord
.. code-block:: modula2
    PROCEDURE ReadWord (VAR f: File; VAR w: WORD) ;
    
    
    (*
       WriteWord - writes one word to a file, f.
                   f.res is set appropriately.
    *)
    
.. index::
   WriteWord
.. code-block:: modula2
    PROCEDURE WriteWord (VAR f: File; w: WORD) ;
    
    
    (*
       ReadChar - reads one character from a file, f.
    *)
    
.. index::
   ReadChar
.. code-block:: modula2
    PROCEDURE ReadChar (VAR f: File; VAR ch: CHAR) ;
    
    
    (*
       WriteChar - writes a character, ch, to a file, f.
                   f.res is set appropriately.
    *)
    
.. index::
   WriteChar
.. code-block:: modula2
    PROCEDURE WriteChar (VAR f: File; ch: CHAR) ;
    
    
    (*
       ReadByte - reads a BYTE, b, from file, f.
                  f.res is set appropriately.
    *)
    
.. index::
   ReadByte
.. code-block:: modula2
    PROCEDURE ReadByte (VAR f: File; VAR b: BYTE) ;
    
    
    (*
       WriteByte - writes one BYTE, b, to a file, f.
                   f.res is set appropriately.
    *)
    
.. index::
   WriteByte
.. code-block:: modula2
    PROCEDURE WriteByte (VAR f: File; b: BYTE) ;
    
    
    (*
       ReadNBytes - reads a sequence of bytes from a file, f.
    *)
    
.. index::
   ReadNBytes
.. code-block:: modula2
    PROCEDURE ReadNBytes (VAR f: File; a: ADDRESS; amount: CARDINAL;
                          VAR actuallyRead: CARDINAL) ;
    
    
    (*
       WriteNBytes - writes a sequence of bytes to file, f.
    *)
    
.. index::
   WriteNBytes
.. code-block:: modula2
    PROCEDURE WriteNBytes (VAR f: File; a: ADDRESS; amount: CARDINAL;
                           VAR actuallyWritten: CARDINAL) ;
    
    
    (*
       Again - returns the last character read to the internal buffer
               so that it can be read again.
    *)
    
.. index::
   Again
.. code-block:: modula2
    PROCEDURE Again (VAR f: File) ;
    
    
    (*
       SetRead - puts the file, f, into the read state.
                 The file position is unchanged.
    *)
    
.. index::
   SetRead
.. code-block:: modula2
    PROCEDURE SetRead (VAR f: File) ;
    
    
    (*
       SetWrite - puts the file, f, into the write state.
                  The file position is unchanged.
    *)
    
.. index::
   SetWrite
.. code-block:: modula2
    PROCEDURE SetWrite (VAR f: File) ;
    
    
    (*
       SetModify - puts the file, f, into the modify state.
                   The file position is unchanged but the file can be
                   read and written.
    *)
    
.. index::
   SetModify
.. code-block:: modula2
    PROCEDURE SetModify (VAR f: File) ;
    
    
    (*
       SetOpen - places a file, f, into the open state. The file may
                 have been in the read/write/modify state before and
                 in which case the previous buffer contents are flushed
                 and the file state is reset to open. The position is
                 unaltered.
    *)
    
.. index::
   SetOpen
.. code-block:: modula2
    PROCEDURE SetOpen (VAR f: File) ;
    
    
    (*
       Reset - places a file, f, into the open state and reset the
               position to the start of the file.
    *)
    
.. index::
   Reset
.. code-block:: modula2
    PROCEDURE Reset (VAR f: File) ;
    
    
    (*
       SetPos - lseek to a position within a file.
    *)
    
.. index::
   SetPos
.. code-block:: modula2
    PROCEDURE SetPos (VAR f: File; high, low: CARDINAL) ;
    
    
    (*
       GetPos - return the position within a file.
    *)
    
.. index::
   GetPos
.. code-block:: modula2
    PROCEDURE GetPos (VAR f: File; VAR high, low: CARDINAL) ;
    
    
    (*
       Length - returns the length of file, in, high, and, low.
    *)
    
.. index::
   Length
.. code-block:: modula2
    PROCEDURE Length (VAR f: File; VAR high, low: CARDINAL) ;
    
    
    (*
       Doio - effectively flushes a file in write mode, rereads the
              current buffer from disk if in read mode and writes
              and rereads the buffer if in modify mode.
    *)
    
.. index::
   Doio
.. code-block:: modula2
    PROCEDURE Doio (VAR f: File) ;
    
    
    (*
       FileNameChar - checks to see whether the character, ch, is
                      legal in a filename. nul is returned if the
                      character was illegal.
    *)
    
.. index::
   FileNameChar
.. code-block:: modula2
    PROCEDURE FileNameChar (ch: CHAR) ;
    
    
    END FileSystem.

@c @node gm2-libs-pim/FloatingUtilities, gm2-libs-pim/InOut, gm2-libs-pim/FileSystem, PIM and Logitech 3.0 Compatible
gm2-libs-pim/FloatingUtilities
------------------------------

.. code-block:: modula2
    DEFINITION MODULE FloatingUtilities ;

    EXPORT QUALIFIED Frac, Round, Float, Trunc,
                     Fracl, Roundl, Floatl, Truncl ;
    
    
    (*
       Frac - returns the fractional component of, r.
    *)
    
.. index::
   Frac
.. code-block:: modula2
    PROCEDURE Frac (r: REAL) : REAL ;
    
    
    (*
       Int - returns the integer part of r. It rounds the value towards zero.
    *)
    
.. index::
   Int
.. code-block:: modula2
    PROCEDURE Int (r: REAL) : INTEGER ;
    
    
    (*
       Round - returns the number rounded to the nearest integer.
    *)
    
.. index::
   Round
.. code-block:: modula2
    PROCEDURE Round (r: REAL) : INTEGER ;
    
    
    (*
       Float - returns a REAL value corresponding to, i.
    *)
    
.. index::
   Float
.. code-block:: modula2
    PROCEDURE Float (i: INTEGER) : REAL ;
    
    
    (*
       Trunc - round to the nearest integer not larger in absolute
               value.
    *)
    
.. index::
   Trunc
.. code-block:: modula2
    PROCEDURE Trunc (r: REAL) : INTEGER ;
    
    
    (*
       Fracl - returns the fractional component of, r.
    *)
    
.. index::
   Fracl
.. code-block:: modula2
    PROCEDURE Fracl (r: LONGREAL) : LONGREAL ;
    
    
    (*
       Intl - returns the integer part of r. It rounds the value towards zero.
    *)
    
.. index::
   Intl
.. code-block:: modula2
    PROCEDURE Intl (r: LONGREAL) : LONGINT ;
    
    
    (*
       Roundl - returns the number rounded to the nearest integer.
    *)
    
.. index::
   Roundl
.. code-block:: modula2
    PROCEDURE Roundl (r: LONGREAL) : LONGINT ;
    
    
    (*
       Floatl - returns a REAL value corresponding to, i.
    *)
    
.. index::
   Floatl
.. code-block:: modula2
    PROCEDURE Floatl (i: INTEGER) : LONGREAL ;
    
    
    (*
       Truncl - round to the nearest integer not larger in absolute
                value.
    *)
    
.. index::
   Truncl
.. code-block:: modula2
    PROCEDURE Truncl (r: LONGREAL) : LONGINT ;
    
    
    END FloatingUtilities.

@c @node gm2-libs-pim/InOut, gm2-libs-pim/Keyboard, gm2-libs-pim/FloatingUtilities, PIM and Logitech 3.0 Compatible
gm2-libs-pim/InOut
------------------

.. code-block:: modula2
    DEFINITION MODULE InOut ;

    IMPORT ASCII ;
    FROM DynamicStrings IMPORT String ;
    EXPORT QUALIFIED EOL, Done, termCH, OpenInput, OpenOutput,
                     CloseInput, CloseOutput,
                     Read, ReadString, ReadInt, ReadCard,
                     Write, WriteLn, WriteString, WriteInt, WriteCard,
                     WriteOct, WriteHex,
                     ReadS, WriteS ;
    
    CONST
.. index::
   pair: EOL; (const)
.. code-block:: modula2
       EOL = ASCII.EOL ;
    
    VAR
.. index::
   pair: Done; (var)
.. code-block:: modula2
       Done  : BOOLEAN ;
.. index::
   pair: termCH; (var)
.. code-block:: modula2
       termCH: CHAR ;
    
    
    (*
       OpenInput - reads a string from stdin as the filename for reading.
                   If the filename ends with `.' then it appends the defext
                   extension. The global variable Done is set if all
                   was successful.
    *)
    
.. index::
   OpenInput
.. code-block:: modula2
    PROCEDURE OpenInput (defext: ARRAY OF CHAR) ;
    
    
    (*
       CloseInput - closes an opened input file and returns input back to
                    StdIn.
    *)
    
.. index::
   CloseInput
.. code-block:: modula2
    PROCEDURE CloseInput ;
    
    
    (*
       OpenOutput - reads a string from stdin as the filename for writing.
                    If the filename ends with `.' then it appends the defext
                    extension. The global variable Done is set if all
                    was successful.
    *)
    
.. index::
   OpenOutput
.. code-block:: modula2
    PROCEDURE OpenOutput (defext: ARRAY OF CHAR) ;
    
    
    (*
       CloseOutput - closes an opened output file and returns output back to
                     StdOut.
    *)
    
.. index::
   CloseOutput
.. code-block:: modula2
    PROCEDURE CloseOutput ;
    
    
    (*
       Read - reads a single character from the current input file.
              Done is set to FALSE if end of file is reached or an
              error occurs.
    *)
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR ch: CHAR) ;
    
    
    (*
       ReadString - reads a sequence of characters. Leading white space
                    is ignored and the string is terminated with a character
                    <= ' '
    *)
    
.. index::
   ReadString
.. code-block:: modula2
    PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;
    
    
    (*
       WriteString - writes a string to the output file.
    *)
    
.. index::
   WriteString
.. code-block:: modula2
    PROCEDURE WriteString (s: ARRAY OF CHAR) ;
    
    
    (*
       Write - writes out a single character, ch, to the current output file.
    *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (ch: CHAR) ;
    
    
    (*
       WriteLn - writes a newline to the output file.
    *)
    
.. index::
   WriteLn
.. code-block:: modula2
    PROCEDURE WriteLn ;
    
    
    (*
       ReadInt - reads a string and converts it into an INTEGER, x.
                 Done is set if an INTEGER is read.
    *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (VAR x: INTEGER) ;
    
    
    (*
       ReadInt - reads a string and converts it into an INTEGER, x.
                 Done is set if an INTEGER is read.
    *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (VAR x: CARDINAL) ;
    
    
    (*
       WriteCard - writes the CARDINAL, x, to the output file. It ensures
                   that the number occupies, n, characters. Leading spaces
                   are added if required.
    *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (x, n: CARDINAL) ;
    
    
    (*
       WriteInt - writes the INTEGER, x, to the output file. It ensures
                  that the number occupies, n, characters. Leading spaces
                  are added if required.
    *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (x: INTEGER; n: CARDINAL) ;
    
    
    (*
       WriteOct - writes the CARDINAL, x, to the output file in octal.
                  It ensures that the number occupies, n, characters.
                  Leading spaces are added if required.
    *)
    
.. index::
   WriteOct
.. code-block:: modula2
    PROCEDURE WriteOct (x, n: CARDINAL) ;
    
    
    (*
       WriteHex - writes the CARDINAL, x, to the output file in hexadecimal.
                  It ensures that the number occupies, n, characters.
                  Leading spaces are added if required.
    *)
    
.. index::
   WriteHex
.. code-block:: modula2
    PROCEDURE WriteHex (x, n: CARDINAL) ;
    
    
    (*
       ReadS - returns a string which has is a sequence of characters.
               Leading white space is ignored and string is terminated
               with a character <= ' '.
    *)
    
.. index::
   ReadS
.. code-block:: modula2
    PROCEDURE ReadS () : String ;
    
    
    (*
       WriteS - writes a String to the output device.
                It returns the string, s.
    *)
    
.. index::
   WriteS
.. code-block:: modula2
    PROCEDURE WriteS (s: String) : String ;
    
    
    END InOut.

@c @node gm2-libs-pim/Keyboard, gm2-libs-pim/LongIO, gm2-libs-pim/InOut, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Keyboard
---------------------

.. code-block:: modula2
    DEFINITION MODULE Keyboard ;

    EXPORT QUALIFIED Read, KeyPressed ;
    
    
    (*
       Read - reads a character from StdIn. If necessary it will wait
              for a key to become present on StdIn.
    *)
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR ch: CHAR) ;
    
    
    (*
       KeyPressed - returns TRUE if a character can be read from StdIn
                    without blocking the caller.
    *)
    
.. index::
   KeyPressed
.. code-block:: modula2
    PROCEDURE KeyPressed () : BOOLEAN ;
    
    
    END Keyboard.

@c @node gm2-libs-pim/LongIO, gm2-libs-pim/NumberConversion, gm2-libs-pim/Keyboard, PIM and Logitech 3.0 Compatible
gm2-libs-pim/LongIO
-------------------

.. code-block:: modula2
    DEFINITION MODULE LongIO ;

    EXPORT QUALIFIED Done, ReadLongInt, WriteLongInt ;
    
    VAR
.. index::
   pair: Done; (var)
.. code-block:: modula2
       Done: BOOLEAN ;
    
.. index::
   ReadLongInt
.. code-block:: modula2
    PROCEDURE ReadLongInt (VAR i: LONGINT) ;
.. index::
   WriteLongInt
.. code-block:: modula2
    PROCEDURE WriteLongInt (i: LONGINT; n: CARDINAL) ;
    
    
    END LongIO.

@c @node gm2-libs-pim/NumberConversion, gm2-libs-pim/Random, gm2-libs-pim/LongIO, PIM and Logitech 3.0 Compatible
gm2-libs-pim/NumberConversion
-----------------------------

.. code-block:: modula2
    DEFINITION MODULE NumberConversion ;

(* --fixme-- finish this.  *)
    
    END NumberConversion.

@c @node gm2-libs-pim/Random, gm2-libs-pim/RealConversions, gm2-libs-pim/NumberConversion, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Random
-------------------

.. code-block:: modula2
    DEFINITION MODULE Random ;

    FROM SYSTEM IMPORT BYTE ;
    EXPORT QUALIFIED Randomize, RandomInit, RandomBytes, RandomCard, RandomInt, RandomReal, RandomLongReal ;
    
    
    (*
       Randomize - initialize the random number generator with a seed
                   based on the microseconds.
    *)
    
.. index::
   Randomize
.. code-block:: modula2
    PROCEDURE Randomize ;
    
    
    (*
       RandomInit - initialize the random number generator with value, seed.
    *)
    
.. index::
   RandomInit
.. code-block:: modula2
    PROCEDURE RandomInit (seed: CARDINAL) ;
    
    
    (*
       RandomBytes - fills in an array with random values.
    *)
    
.. index::
   RandomBytes
.. code-block:: modula2
    PROCEDURE RandomBytes (VAR a: ARRAY OF BYTE) ;
    
    
    (*
       RandomInt - return an INTEGER in the range 0..bound-1
    *)
    
.. index::
   RandomInt
.. code-block:: modula2
    PROCEDURE RandomInt (bound: INTEGER) : INTEGER ;
    
    
    (*
       RandomCard - return a CARDINAL in the range 0..bound-1
    *)
    
.. index::
   RandomCard
.. code-block:: modula2
    PROCEDURE RandomCard (bound: CARDINAL) : CARDINAL ;
    
    
    (*
       RandomReal - return a REAL number in the range 0.0..1.0
    *)
    
.. index::
   RandomReal
.. code-block:: modula2
    PROCEDURE RandomReal () : REAL ;
    
    
    (*
       RandomLongReal - return a LONGREAL number in the range 0.0..1.0
    *)
    
.. index::
   RandomLongReal
.. code-block:: modula2
    PROCEDURE RandomLongReal () : LONGREAL ;
    
    
    END Random.

@c @node gm2-libs-pim/RealConversions, gm2-libs-pim/RealInOut, gm2-libs-pim/Random, PIM and Logitech 3.0 Compatible
gm2-libs-pim/RealConversions
----------------------------

.. code-block:: modula2
    DEFINITION MODULE RealConversions ;

    EXPORT QUALIFIED SetNoOfExponentDigits,
                     RealToString, StringToReal,
                     LongRealToString, StringToLongReal ;
    
    
    (*
       SetNoOfExponentDigits - sets the number of exponent digits to be
                               used during future calls of LongRealToString
                               and RealToString providing that the width
                               is sufficient.
                               If this value is set to 0 (the default) then
                               the number digits used is the minimum necessary.
    *)
    
.. index::
   SetNoOfExponentDigits
.. code-block:: modula2
    PROCEDURE SetNoOfExponentDigits (places: CARDINAL) ;
    
    
    (*
       RealToString - converts a real, r, into a right justified string, str.
                      The number of digits to the right of the decimal point
                      is given in, digits.  The value, width, represents the
                      maximum number of characters to be used in the string,
                      str.
    
                      If digits is negative then exponent notation is used
                      whereas if digits is positive then fixed point notation
                      is used.
    
                      If, r, is less than 0.0 then a '-' preceeds the value,
                      str.  However, if, r, is >= 0.0 a '+' is not added.
    
                      If the conversion of, r, to a string requires more
                      than, width, characters then the string, str, is set
                      to a nul string and, ok is assigned FALSE.
    
                      For fixed point notation the minimum width required is
                      ABS(width)+8
    
                      For exponent notation the minimum width required is
                      ABS(digits)+2+log10(magnitude).
    
                      if r is a NaN then the string 'nan' is returned formatted and
                      ok will be FALSE.
    *)
    
.. index::
   RealToString
.. code-block:: modula2
    PROCEDURE RealToString (r: REAL; digits, width: INTEGER;
                            VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;
    
    
    (*
       LongRealToString - converts a real, r, into a right justified string, str.
                          The number of digits to the right of the decimal point
                          is given in, digits. The value, width, represents the
                          maximum number of characters to be used in the string,
                          str.
    
                          If digits is negative then exponent notation is used
                          whereas if digits is positive then fixed point notation
                          is used.
    
                          If, r, is less than 0.0 then a '-' preceeds the value,
                          str. However, if, r, is >= 0.0 a '+' is not added.
    
                          If the conversion of, r, to a string requires more
                          than, width, characters then the string, str, is set
                          to a nul string and, ok is assigned FALSE.
    
                          For fixed point notation the minimum width required is
                          ABS(width)+8
    
                          For exponent notation the minimum width required is
                          ABS(digits)+2+log10(magnitude).
    
                          Examples:
                          RealToString(100.0, 10, 10, a, ok)       ->  '100.000000'
                          RealToString(100.0, -5, 12, a, ok)       ->  '  1.00000E+2'
    
                          RealToString(123.456789, 10, 10, a, ok)  ->  '123.456789'
                          RealToString(123.456789, -5, 13, a, ok)  ->  '    1.23456E+2'
    
                          RealToString(123.456789, -2, 15, a, ok)  ->  '          1.23E+2'
    
                          if r is a NaN then the string 'nan' is returned formatted and
                          ok will be FALSE.
    *)
    
.. index::
   LongRealToString
.. code-block:: modula2
    PROCEDURE LongRealToString (r: LONGREAL; digits, width: INTEGER;
                                VAR str: ARRAY OF CHAR; VAR ok: BOOLEAN) ;
    
    
    (*
       StringToReal - converts, str, into a REAL, r. The parameter, ok, is
                      set to TRUE if the conversion was successful.
    *)
    
.. index::
   StringToReal
.. code-block:: modula2
    PROCEDURE StringToReal (str: ARRAY OF CHAR; VAR r: REAL; VAR ok: BOOLEAN) ;
    
    
    (*
       StringToLongReal - converts, str, into a LONGREAL, r. The parameter, ok, is
                          set to TRUE if the conversion was successful.
    *)
    
.. index::
   StringToLongReal
.. code-block:: modula2
    PROCEDURE StringToLongReal (str: ARRAY OF CHAR; VAR r: LONGREAL; VAR ok: BOOLEAN) ;
    
    
    END RealConversions.

@c @node gm2-libs-pim/RealInOut, gm2-libs-pim/Strings, gm2-libs-pim/RealConversions, PIM and Logitech 3.0 Compatible
gm2-libs-pim/RealInOut
----------------------

.. code-block:: modula2
    DEFINITION MODULE RealInOut ;

    EXPORT QUALIFIED SetNoOfDecimalPlaces,
                     ReadReal, WriteReal, WriteRealOct,
                     ReadLongReal, WriteLongReal, WriteLongRealOct,
                     ReadShortReal, WriteShortReal, WriteShortRealOct,
                     Done ;
    
    CONST
.. index::
   pair: DefaultDecimalPlaces; (const)
.. code-block:: modula2
       DefaultDecimalPlaces = 6 ;
    
    VAR
.. index::
   pair: Done; (var)
.. code-block:: modula2
       Done: BOOLEAN ;
    
    
    (*
       SetNoOfDecimalPlaces - number of decimal places WriteReal and
                              WriteLongReal should emit.  This procedure
                              can be used to override the default
                              DefaultDecimalPlaces constant.
    *)
    
.. index::
   SetNoOfDecimalPlaces
.. code-block:: modula2
    PROCEDURE SetNoOfDecimalPlaces (places: CARDINAL) ;
    
    
    (*
       ReadReal - reads a real number, legal syntaxes include:
                  100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
    *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (VAR x: REAL) ;
    
    
    (*
       WriteReal - writes a real to the terminal. The real number
                   is right justified and, n, is the minimum field
                   width.
    *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (x: REAL; n: CARDINAL) ;
    
    
    (*
       WriteRealOct - writes the real to terminal in octal words.
    *)
    
.. index::
   WriteRealOct
.. code-block:: modula2
    PROCEDURE WriteRealOct (x: REAL) ;
    
    
    (*
       ReadLongReal - reads a LONGREAL number, legal syntaxes include:
                      100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
    *)
    
.. index::
   ReadLongReal
.. code-block:: modula2
    PROCEDURE ReadLongReal (VAR x: LONGREAL) ;
    
    
    (*
       WriteLongReal - writes a LONGREAL to the terminal. The real number
                       is right justified and, n, is the minimum field
                       width.
    *)
    
.. index::
   WriteLongReal
.. code-block:: modula2
    PROCEDURE WriteLongReal (x: LONGREAL; n: CARDINAL) ;
    
    
    (*
       WriteLongRealOct - writes the LONGREAL to terminal in octal words.
    *)
    
.. index::
   WriteLongRealOct
.. code-block:: modula2
    PROCEDURE WriteLongRealOct (x: LONGREAL) ;
    
    
    (*
       ReadShortReal - reads a SHORTREAL number, legal syntaxes include:
                       100, 100.0, 100e0, 100E0, 100E-1, E2, +1E+2, 1e+2
    *)
    
.. index::
   ReadShortReal
.. code-block:: modula2
    PROCEDURE ReadShortReal (VAR x: SHORTREAL) ;
    
    
    (*
       WriteShortReal - writes a SHORTREAL to the terminal. The real number
                        is right justified and, n, is the minimum field
                        width.
    *)
    
.. index::
   WriteShortReal
.. code-block:: modula2
    PROCEDURE WriteShortReal (x: SHORTREAL; n: CARDINAL) ;
    
    
    (*
       WriteShortRealOct - writes the SHORTREAL to terminal in octal words.
    *)
    
.. index::
   WriteShortRealOct
.. code-block:: modula2
    PROCEDURE WriteShortRealOct (x: SHORTREAL) ;
    
    
    END RealInOut.

@c @node gm2-libs-pim/Strings, gm2-libs-pim/Termbase, gm2-libs-pim/RealInOut, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Strings
--------------------

.. code-block:: modula2
    DEFINITION MODULE Strings ;

    EXPORT QUALIFIED Assign, Insert, Delete, Pos, Copy, ConCat, Length,
                     CompareStr ;
    
    (*
       Assign - dest := source.
    *)
    
.. index::
   Assign
.. code-block:: modula2
    PROCEDURE Assign (VAR dest: ARRAY OF CHAR; source: ARRAY OF CHAR) ;
    
    
    (*
       Insert - insert the string, substr, into str at position, index.
                substr, is added to the end of, str, if, index >= length(str)
    *)
    
.. index::
   Insert
.. code-block:: modula2
    PROCEDURE Insert (substr: ARRAY OF CHAR; VAR str: ARRAY OF CHAR;
                      index: CARDINAL) ;
    
    
    (*
       Delete - delete len characters from, str, starting at, index.
    *)
    
.. index::
   Delete
.. code-block:: modula2
    PROCEDURE Delete (VAR str: ARRAY OF CHAR; index: CARDINAL; length: CARDINAL) ;
    
    
    (*
       Pos - return the first position of, substr, in, str.
    *)
    
.. index::
   Pos
.. code-block:: modula2
    PROCEDURE Pos (substr, str: ARRAY OF CHAR) : CARDINAL ;
    
    
    (*
       Copy - copy at most, length, characters in, substr, to, str,
              starting at position, index.
    *)
    
.. index::
   Copy
.. code-block:: modula2
    PROCEDURE Copy (str: ARRAY OF CHAR;
                    index, length: CARDINAL; VAR result: ARRAY OF CHAR) ;
    
    (*
       ConCat - concatenates two strings, s1, and, s2
                and places the result into, dest.
    *)
    
.. index::
   ConCat
.. code-block:: modula2
    PROCEDURE ConCat (s1, s2: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR) ;
    
    
    (*
       Length - return the length of string, s.
    *)
    
.. index::
   Length
.. code-block:: modula2
    PROCEDURE Length (s: ARRAY OF CHAR) : CARDINAL ;
    
    
    (*
       CompareStr - compare two strings, left, and, right.
    *)
    
.. index::
   CompareStr
.. code-block:: modula2
    PROCEDURE CompareStr (left, right: ARRAY OF CHAR) : INTEGER ;
    
    
    END Strings.

@c @node gm2-libs-pim/Termbase, gm2-libs-pim/Terminal, gm2-libs-pim/Strings, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Termbase
---------------------

.. code-block:: modula2
    DEFINITION MODULE Termbase ;

(*
   Initially the read routines from Keyboard and the
   write routine from Display is assigned to the Read,
   KeyPressed and Write procedures.
*)
    
    EXPORT QUALIFIED ReadProcedure, StatusProcedure, WriteProcedure,
                     AssignRead, AssignWrite, UnAssignRead, UnAssignWrite,
                     Read, KeyPressed, Write ;
    
    TYPE
.. index::
   pair: ReadProcedure; (type)
.. code-block:: modula2
       ReadProcedure = PROCEDURE (VAR CHAR) ;
.. index::
   pair: WriteProcedure; (type)
.. code-block:: modula2
       WriteProcedure = PROCEDURE (CHAR) ;
.. index::
   pair: StatusProcedure; (type)
.. code-block:: modula2
       StatusProcedure = PROCEDURE () : BOOLEAN ;
    
    
    (*
       AssignRead - assigns a read procedure and status procedure for terminal
                    input. Done is set to TRUE if successful. Subsequent
                    Read and KeyPressed calls are mapped onto the user supplied
                    procedures. The previous read and status procedures are
                    uncovered and reused after UnAssignRead is called.
    *)
    
.. index::
   AssignRead
.. code-block:: modula2
    PROCEDURE AssignRead (rp: ReadProcedure; sp: StatusProcedure;
                          VAR Done: BOOLEAN) ;
    
    
    (*
       UnAssignRead - undo the last call to AssignRead and set Done to TRUE
                      on success.
    *)
    
.. index::
   UnAssignRead
.. code-block:: modula2
    PROCEDURE UnAssignRead (VAR Done: BOOLEAN) ;
    
    
    (*
       Read - reads a single character using the currently active read
              procedure.
    *)
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR ch: CHAR) ;
    
    
    (*
       KeyPressed - returns TRUE if a character is available to be read.
    *)
    
.. index::
   KeyPressed
.. code-block:: modula2
    PROCEDURE KeyPressed () : BOOLEAN ;
    
    
    (*
       AssignWrite - assigns a write procedure for terminal output.
                     Done is set to TRUE if successful. Subsequent
                     Write calls are mapped onto the user supplied
                     procedure. The previous write procedure is
                     uncovered and reused after UnAssignWrite is called.
    *)
    
.. index::
   AssignWrite
.. code-block:: modula2
    PROCEDURE AssignWrite (wp: WriteProcedure; VAR Done: BOOLEAN) ;
    
    
    (*
       UnAssignWrite - undo the last call to AssignWrite and set Done to TRUE
                       on success.
    *)
    
.. index::
   UnAssignWrite
.. code-block:: modula2
    PROCEDURE UnAssignWrite (VAR Done: BOOLEAN) ;
    
    
    (*
       Write - writes a single character using the currently active write
               procedure.
    *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (VAR ch: CHAR) ;
    
    
    END Termbase.

@c @node gm2-libs-pim/Terminal, gm2-libs-pim/TimeDate, gm2-libs-pim/Termbase, PIM and Logitech 3.0 Compatible
gm2-libs-pim/Terminal
---------------------

.. code-block:: modula2
    DEFINITION MODULE Terminal ;

(*
   It provides simple terminal input output
   routines which all utilize the TermBase module.
*)
    
    EXPORT QUALIFIED Read, KeyPressed, ReadAgain, ReadString, Write,
                     WriteString, WriteLn ;
    
    
    (*
       Read - reads a single character.
    *)
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR ch: CHAR) ;
    
    
    (*
       KeyPressed - returns TRUE if a character can be read without blocking
                    the caller.
    *)
    
.. index::
   KeyPressed
.. code-block:: modula2
    PROCEDURE KeyPressed () : BOOLEAN ;
    
    
    (*
       ReadString - reads a sequence of characters.
                    Tabs are expanded into 8 spaces and <cr> or <lf> terminates
                    the string.
    *)
    
.. index::
   ReadString
.. code-block:: modula2
    PROCEDURE ReadString (VAR s: ARRAY OF CHAR) ;
    
    
    (*
       ReadAgain - makes the last character readable again.
    *)
    
.. index::
   ReadAgain
.. code-block:: modula2
    PROCEDURE ReadAgain ;
    
    
    (*
       Write - writes a single character to the Termbase module.
    *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (ch: CHAR) ;
    
    
    (*
       WriteString - writes out a string which is terminated by a <nul>
                     character or the end of string HIGH(s).
    *)
    
.. index::
   WriteString
.. code-block:: modula2
    PROCEDURE WriteString (s: ARRAY OF CHAR) ;
    
    
    (*
       WriteLn - writes a lf character.
    *)
    
.. index::
   WriteLn
.. code-block:: modula2
    PROCEDURE WriteLn ;
    
    
    END Terminal.

@c @node gm2-libs-pim/TimeDate, , gm2-libs-pim/Terminal, PIM and Logitech 3.0 Compatible
gm2-libs-pim/TimeDate
---------------------

.. code-block:: modula2
    DEFINITION MODULE TimeDate ;

(*
   Legacy compatibility - you are advised to use cleaner
   designed modules based on 'man 3 strtime'
   and friends for new projects as the day value here is ugly.
   [it was mapped onto MSDOS pre 2000].
*)
    
    EXPORT QUALIFIED Time, GetTime, SetTime, CompareTime, TimeToZero,
                     TimeToString ;
    
    TYPE
    (*
       day holds:  bits 0..4 = day of month (1..31)
                        5..8 = month of year (1..12)
                        9..  = year - 1900
       minute holds:    hours * 60 + minutes
       millisec holds:  seconds * 1000 + millisec
                        which is reset to 0 every minute
    *)
    
       Time = RECORD
                 day, minute, millisec: CARDINAL ;
              END ;
    
    
    (*
       GetTime - returns the current date and time.
    *)
    
.. index::
   GetTime
.. code-block:: modula2
    PROCEDURE GetTime (VAR curTime: Time) ;
    
    
    (*
       SetTime - does nothing, but provides compatibility with
                 the Logitech-3.0 library.
    *)
    
.. index::
   SetTime
.. code-block:: modula2
    PROCEDURE SetTime (curTime: Time) ;
    
    
    (*
       CompareTime - compare two dates and time which returns:
    
                     -1  if t1 < t2
                      0  if t1 = t2
                      1  if t1 > t2
    *)
    
.. index::
   CompareTime
.. code-block:: modula2
    PROCEDURE CompareTime (t1, t2: Time) : INTEGER ;
    
    
    (*
       TimeToZero - initializes, t, to zero.
    *)
    
.. index::
   TimeToZero
.. code-block:: modula2
    PROCEDURE TimeToZero (VAR t: Time) ;
    
    
    (*
       TimeToString - convert time, t, to a string.
                      The string, s, should be at least 19 characters
                      long and the returned string will be
    
                      yyyy-mm-dd hh:mm:ss
    *)
    
.. index::
   TimeToString
.. code-block:: modula2
    PROCEDURE TimeToString (t: Time; VAR s: ARRAY OF CHAR) ;
    
    
    END TimeDate.


@c ------------------------------------------------------------
@c @node PIM coroutine support, M2 ISO Libraries, PIM and Logitech 3.0 Compatible, Libraries
PIM coroutine support
=====================

@menu
* gm2-libs-coroutines/Debug::Debug.def
* gm2-libs-coroutines/Executive::Executive.def
* gm2-libs-coroutines/KeyBoardLEDs::KeyBoardLEDs.def
* gm2-libs-coroutines/SYSTEM::SYSTEM.def
* gm2-libs-coroutines/TimerHandler::TimerHandler.def
@end menu

@c @node gm2-libs-coroutines/Debug, gm2-libs-coroutines/Executive, , PIM coroutine support
gm2-libs-coroutines/Debug
-------------------------

.. code-block:: modula2
    DEFINITION MODULE Debug ;

(*
    Description: provides some simple debugging routines.
*)
    
    EXPORT QUALIFIED Halt, DebugString, PushOutput ;
    
    TYPE
.. index::
   pair: WriteP; (type)
.. code-block:: modula2
       WriteP = PROCEDURE (CHAR) ;
    
    
    (*
       Halt - writes a message in the format:
              Module:Line:Message
    
              It then terminates by calling HALT.
    *)
    
.. index::
   Halt
.. code-block:: modula2
    PROCEDURE Halt (File    : ARRAY OF CHAR;
                    LineNo  : CARDINAL;
                    Function,
                    Message : ARRAY OF CHAR) ;
    
    
    (*
       DebugString - writes a string to the debugging device (Scn.Write).
                     It interprets \n as carriage return, linefeed.
    *)
    
.. index::
   DebugString
.. code-block:: modula2
    PROCEDURE DebugString (a: ARRAY OF CHAR) ;
    
    
    (*
       PushOutput - pushes the output procedure, p, which is used Debug.
    *)
    
.. index::
   PushOutput
.. code-block:: modula2
    PROCEDURE PushOutput (p: WriteP) ;
    
    
    (*
       PopOutput - pops the current output procedure from the stack.
    *)
    
.. index::
   PopOutput
.. code-block:: modula2
    PROCEDURE PopOutput ;
    
    
    END Debug.

@c @node gm2-libs-coroutines/Executive, gm2-libs-coroutines/KeyBoardLEDs, gm2-libs-coroutines/Debug, PIM coroutine support
gm2-libs-coroutines/Executive
-----------------------------

.. code-block:: modula2
    DEFINITION MODULE Executive ;

    EXPORT QUALIFIED SEMAPHORE, DESCRIPTOR,
                     InitProcess, KillProcess, Resume, Suspend, InitSemaphore,
                     Wait, Signal, WaitForIO, Ps, GetCurrentProcess,
                     RotateRunQueue, ProcessName, DebugProcess ;
    
    TYPE
.. index::
   pair: SEMAPHORE; (type)
.. code-block:: modula2
       SEMAPHORE ;         (* defines Dijkstra's semaphores *)
.. index::
   pair: DESCRIPTOR; (type)
.. code-block:: modula2
       DESCRIPTOR ;        (* handle onto a process         *)
    
    
    (*
       InitProcess - initializes a process which is held in the suspended
                     state. When the process is resumed it will start executing
                     procedure, p. The process has a maximum stack size of,
                     StackSize, bytes and its textual name is, Name.
                     The StackSize should be at least 5000 bytes.
    *)
    
.. index::
   InitProcess
.. code-block:: modula2
    PROCEDURE InitProcess (p: PROC; StackSize: CARDINAL;
                           Name: ARRAY OF CHAR) : DESCRIPTOR ;
    
    
    (*
       KillProcess - kills the current process. Notice that if InitProcess
                     is called again, it might reuse the DESCRIPTOR of the
                     killed process. It is the responsibility of the caller
                     to ensure all other processes understand this process
                     is different.
    *)
    
.. index::
   KillProcess
.. code-block:: modula2
    PROCEDURE KillProcess ;
    
    
    (*
       Resume - resumes a suspended process. If all is successful then the process, p,
                is returned. If it fails then NIL is returned.
    *)
    
.. index::
   Resume
.. code-block:: modula2
    PROCEDURE Resume (d: DESCRIPTOR) : DESCRIPTOR ;
    
    
    (*
       Suspend - suspend the calling process.
                 The process can only continue running if another process
                 Resumes it.
    *)
    
.. index::
   Suspend
.. code-block:: modula2
    PROCEDURE Suspend ;
    
    
    (*
       InitSemaphore - creates a semaphore whose initial value is, v, and
                       whose name is, Name.
    *)
    
.. index::
   InitSemaphore
.. code-block:: modula2
    PROCEDURE InitSemaphore (v: CARDINAL; Name: ARRAY OF CHAR) : SEMAPHORE ;
    
    
    (*
       Wait - performs dijkstra's P operation on a semaphore.
              A process which calls this procedure will
              wait until the value of the semaphore is > 0
              and then it will decrement this value.
    *)
    
.. index::
   Wait
.. code-block:: modula2
    PROCEDURE Wait (s: SEMAPHORE) ;
    
    
    (*
       Signal - performs dijkstra's V operation on a semaphore.
                A process which calls the procedure will increment
                the semaphores value.
    *)
    
.. index::
   Signal
.. code-block:: modula2
    PROCEDURE Signal (s: SEMAPHORE) ;
    
    
    (*
       WaitForIO - waits for an interrupt to occur on vector, VectorNo.
    *)
    
.. index::
   WaitForIO
.. code-block:: modula2
    PROCEDURE WaitForIO (VectorNo: CARDINAL) ;
    
    
    (*
       Ps - displays a process list together with process status.
    *)
    
.. index::
   Ps
.. code-block:: modula2
    PROCEDURE Ps ;
    
    
    (*
       GetCurrentProcess - returns the descriptor of the current running
                           process.
    *)
    
.. index::
   GetCurrentProcess
.. code-block:: modula2
    PROCEDURE GetCurrentProcess () : DESCRIPTOR ;
    
    
    (*
       RotateRunQueue - rotates the process run queue.
                        It does not call the scheduler.
    *)
    
.. index::
   RotateRunQueue
.. code-block:: modula2
    PROCEDURE RotateRunQueue ;
    
    
    (*
       ProcessName - displays the name of process, d, through
                     DebugString.
    *)
    
.. index::
   ProcessName
.. code-block:: modula2
    PROCEDURE ProcessName (d: DESCRIPTOR) ;
    
    
    (*
       DebugProcess - gdb debug handle to enable users to debug deadlocked
                      semaphore processes.
    *)
    
.. index::
   DebugProcess
.. code-block:: modula2
    PROCEDURE DebugProcess (d: DESCRIPTOR) ;
    
    
    END Executive.

@c @node gm2-libs-coroutines/KeyBoardLEDs, gm2-libs-coroutines/SYSTEM, gm2-libs-coroutines/Executive, PIM coroutine support
gm2-libs-coroutines/KeyBoardLEDs
--------------------------------

.. code-block:: modula2
    DEFINITION MODULE KeyBoardLEDs ;

    
    EXPORT QUALIFIED SwitchLeds,
                     SwitchScroll, SwitchNum, SwitchCaps ;
    
    
    (*
       SwitchLeds - switch the keyboard LEDs to the state defined
                    by the BOOLEAN variables. TRUE = ON.
    *)
    
.. index::
   SwitchLeds
.. code-block:: modula2
    PROCEDURE SwitchLeds (NumLock, CapsLock, ScrollLock: BOOLEAN) ;
    
    
    (*
       SwitchScroll - switchs the scroll LED on or off.
    *)
    
.. index::
   SwitchScroll
.. code-block:: modula2
    PROCEDURE SwitchScroll (Scroll: BOOLEAN) ;
    
    
    (*
       SwitchNum - switches the Num LED on or off.
    *)
    
.. index::
   SwitchNum
.. code-block:: modula2
    PROCEDURE SwitchNum (Num: BOOLEAN) ;
    
    
    (*
       SwitchCaps - switches the Caps LED on or off.
    *)
    
.. index::
   SwitchCaps
.. code-block:: modula2
    PROCEDURE SwitchCaps (Caps: BOOLEAN) ;
    
    
    END KeyBoardLEDs.

@c @node gm2-libs-coroutines/SYSTEM, gm2-libs-coroutines/TimerHandler, gm2-libs-coroutines/KeyBoardLEDs, PIM coroutine support
gm2-libs-coroutines/SYSTEM
--------------------------

.. code-block:: modula2
    DEFINITION MODULE SYSTEM ;

(* This module is designed to be used on a native operating system
   rather than an embedded system as it implements the coroutine
   primitives TRANSFER, IOTRANSFER and
   NEWPROCESS through the GNU Pthread library.  *)
    
    FROM COROUTINES IMPORT PROTECTION ;
    
    EXPORT QUALIFIED (* the following are built into the compiler: *)
                     ADDRESS, WORD, BYTE, CSIZE_T, CSSIZE_T, (* @SYSTEM_DATATYPES@ *)
                     ADR, TSIZE, ROTATE, SHIFT, THROW, TBITSIZE,
                     (* SIZE is exported depending upon -fpim2 and
                        -fpedantic.  *)
                     (* The rest are implemented in SYSTEM.mod.  *)
                     PROCESS, TRANSFER, NEWPROCESS, IOTRANSFER,
                     LISTEN,
                     ListenLoop, TurnInterrupts,
                     (* Internal GM2 compiler functions.  *)
                     ShiftVal, ShiftLeft, ShiftRight,
                     RotateVal, RotateLeft, RotateRight ;
    
    
    TYPE
.. index::
   pair: PROCESS; (type)
.. code-block:: modula2
       PROCESS  = RECORD
                     context: INTEGER ;
.. index::
   pair: END; (type)
.. code-block:: modula2
                  END ;
    (* all the following types are declared internally to gm2
       @SYSTEM_TYPES@
    *)
    
    
    (*
       TRANSFER - save the current volatile environment into, p1.
                  Restore the volatile environment from, p2.
    *)
    
.. index::
   TRANSFER
.. code-block:: modula2
    PROCEDURE TRANSFER (VAR p1: PROCESS; p2: PROCESS) ;
    
    
    (*
       NEWPROCESS - p is a parameterless procedure, a, is the origin of
                    the workspace used for the process stack and containing
                    the volatile environment of the process.  StackSize, is
                    the maximum size of the stack in bytes which can be used
                    by this process.  new, is the new process.
    *)
    
.. index::
   NEWPROCESS
.. code-block:: modula2
    PROCEDURE NEWPROCESS (p: PROC; a: ADDRESS; StackSize: CARDINAL; VAR new: PROCESS) ;
    
    
    (*
       IOTRANSFER - saves the current volatile environment into, First,
                    and restores volatile environment, Second.
                    When an interrupt, InterruptNo, is encountered then
                    the reverse takes place.  (The then current volatile
                    environment is shelved onto Second and First is resumed).
    
                    NOTE: that upon interrupt the Second might not be the
                          same process as that before the original call to
                          IOTRANSFER.
    *)
    
.. index::
   IOTRANSFER
.. code-block:: modula2
    PROCEDURE IOTRANSFER (VAR First, Second: PROCESS; InterruptNo: CARDINAL) ;
    
    
    (*
       LISTEN - briefly listen for any interrupts.
    *)
    
.. index::
   LISTEN
.. code-block:: modula2
    PROCEDURE LISTEN ;
    
    
    (*
       ListenLoop - should be called instead of users writing:
    
                    LOOP
                       LISTEN
                    END
    
                    It performs the same function but yields
                    control back to the underlying operating system
                    via a call to pth_select.
                    It also checks for deadlock.
                    This function returns when an interrupt occurs ie
                    a file descriptor becomes ready or a time event
                    expires.  See the module RTint.
    *)
    
.. index::
   ListenLoop
.. code-block:: modula2
    PROCEDURE ListenLoop ;
    
    
    (*
       TurnInterrupts - switches processor interrupts to the protection
                        level, to.  It returns the old value.
    *)
    
.. index::
   TurnInterrupts
.. code-block:: modula2
    PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;
    
    
    (*
       all the functions below are declared internally to gm2
       ====================================================
    
.. index::
   ADR
.. code-block:: modula2
    PROCEDURE ADR (VAR v: <anytype>): ADDRESS;
      (* Returns the address of variable v. *)
    
.. index::
   SIZE
.. code-block:: modula2
    PROCEDURE SIZE (v: <type>) : ZType;
      (* Returns the number of BYTES used to store a v of
         any specified <type>.  Only available if -fpim2 is used.
      *)
    
.. index::
   TSIZE
.. code-block:: modula2
    PROCEDURE TSIZE (<type>) : CARDINAL;
      (* Returns the number of BYTES used to store a value of the
         specified <type>.
      *)
    
.. index::
   ROTATE
.. code-block:: modula2
    PROCEDURE ROTATE (val: <a set type>;
                      num: INTEGER): <type of first parameter>;
      (* Returns a bit sequence obtained from val by rotating up or down
         (left or right) by the absolute value of num.  The direction is
         down if the sign of num is negative, otherwise the direction is up.
      *)
    
.. index::
   SHIFT
.. code-block:: modula2
    PROCEDURE SHIFT (val: <a set type>;
                     num: INTEGER): <type of first parameter>;
      (* Returns a bit sequence obtained from val by shifting up or down
         (left or right) by the absolute value of num, introducing
         zeros as necessary.  The direction is down if the sign of
         num is negative, otherwise the direction is up.
      *)
    
.. index::
   THROW
.. code-block:: modula2
    PROCEDURE THROW (i: INTEGER) ;
      (*
         THROW is a GNU extension and was not part of the PIM or ISO
         standards.  It throws an exception which will be caught by the EXCEPT
         block (assuming it exists).  This is a compiler builtin function which
         interfaces to the GCC exception handling runtime system.
         GCC uses the term throw, hence the naming distinction between
         the GCC builtin and the Modula-2 runtime library procedure Raise.
         The later library procedure Raise will call SYSTEM.THROW after
         performing various housekeeping activities.
      *)
    
.. index::
   TBITSIZE
.. code-block:: modula2
    PROCEDURE TBITSIZE (<type>) : CARDINAL ;
      (* Returns the minimum number of bits necessary to represent
         <type>.  This procedure function is only useful for determining
         the number of bits used for any type field within a packed RECORD.
         It is not particularly useful elsewhere since <type> might be
         optimized for speed, for example a BOOLEAN could occupy a WORD.
      *)
    *)
    
    (* The following procedures are invoked by GNU Modula-2 to
       shift non word sized set types.  They are not strictly part
       of the core PIM Modula-2, however they are used
       to implement the SHIFT procedure defined above,
       which are in turn used by the Logitech compatible libraries.
    
       Users will access these procedures by using the procedure
       SHIFT above and GNU Modula-2 will map SHIFT onto one of
       the following procedures.
    *)
    
    (*
       ShiftVal - is a runtime procedure whose job is to implement
                  the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
                  inline a SHIFT of a single WORD sized set and will
                  only call this routine for larger sets.
    *)
    
.. index::
   ShiftVal
.. code-block:: modula2
    PROCEDURE ShiftVal (VAR s, d: ARRAY OF BITSET;
                        SetSizeInBits: CARDINAL;
                        ShiftCount: INTEGER) ;
    
    
    (*
       ShiftLeft - performs the shift left for a multi word set.
                   This procedure might be called by the back end of
                   GNU Modula-2 depending whether amount is known at
                   compile time.
    *)
    
.. index::
   ShiftLeft
.. code-block:: modula2
    PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         ShiftCount: CARDINAL) ;
    
    (*
       ShiftRight - performs the shift left for a multi word set.
                    This procedure might be called by the back end of
                    GNU Modula-2 depending whether amount is known at
                    compile time.
    *)
    
.. index::
   ShiftRight
.. code-block:: modula2
    PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         ShiftCount: CARDINAL) ;
    
    
    (*
       RotateVal - is a runtime procedure whose job is to implement
                   the ROTATE procedure of ISO SYSTEM.  GNU Modula-2 will
                   inline a ROTATE of a single WORD (or less)
                   sized set and will only call this routine for
                   larger sets.
    *)
    
.. index::
   RotateVal
.. code-block:: modula2
    PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         RotateCount: INTEGER) ;
    
    
    (*
       RotateLeft - performs the rotate left for a multi word set.
                    This procedure might be called by the back end of
                    GNU Modula-2 depending whether amount is known
                    at compile time.
    *)
    
.. index::
   RotateLeft
.. code-block:: modula2
    PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                          SetSizeInBits: CARDINAL;
                          RotateCount: CARDINAL) ;
    
    
    (*
       RotateRight - performs the rotate right for a multi word set.
                     This procedure might be called by the back end of
                     GNU Modula-2 depending whether amount is known at
                     compile time.
    *)
    
.. index::
   RotateRight
.. code-block:: modula2
    PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                           SetSizeInBits: CARDINAL;
                           RotateCount: CARDINAL) ;
    
    
    END SYSTEM.

@c @node gm2-libs-coroutines/TimerHandler, , gm2-libs-coroutines/SYSTEM, PIM coroutine support
gm2-libs-coroutines/TimerHandler
--------------------------------

.. code-block:: modula2
    DEFINITION MODULE TimerHandler ;

(* It also provides the Executive with a basic round robin scheduler.  *)
    
    EXPORT QUALIFIED TicksPerSecond, GetTicks,
                     EVENT,
                     Sleep, ArmEvent, WaitOn, Cancel, ReArmEvent ;
    
    
    CONST
.. index::
   pair: TicksPerSecond; (const)
.. code-block:: modula2
       TicksPerSecond =   25 ;  (* Number of ticks per second.  *)
    
    TYPE
.. index::
   pair: EVENT; (type)
.. code-block:: modula2
       EVENT ;
    
    
    (*
       GetTicks - returns the number of ticks since boottime.
    *)
    
.. index::
   GetTicks
.. code-block:: modula2
    PROCEDURE GetTicks () : CARDINAL ;
    
    
    (*
       Sleep - suspends the current process for a time, t.
               The time is measured in ticks.
    *)
    
.. index::
   Sleep
.. code-block:: modula2
    PROCEDURE Sleep (t: CARDINAL) ;
    
    
    (*
       ArmEvent - initializes an event, e, to occur at time, t.
                  The time, t, is measured in ticks.
                  The event is NOT placed onto the event queue.
    *)
    
.. index::
   ArmEvent
.. code-block:: modula2
    PROCEDURE ArmEvent (t: CARDINAL) : EVENT ;
    
    
    (*
       WaitOn - places event, e, onto the event queue and then the calling
                process suspends. It is resumed up by either the event
                expiring or the event, e, being cancelled.
                TRUE is returned if the event was cancelled
                FALSE is returned if the event expires.
                The event, e, is always assigned to NIL when the function
                finishes.
    *)
    
.. index::
   WaitOn
.. code-block:: modula2
    PROCEDURE WaitOn (VAR e: EVENT) : BOOLEAN ;
    
    
    (*
       Cancel - cancels the event, e, on the event queue and makes
                the appropriate process runnable again.
                TRUE is returned if the event was cancelled and
                FALSE is returned is the event was not found or
                      no process was waiting on this event.
    *)
    
.. index::
   Cancel
.. code-block:: modula2
    PROCEDURE Cancel (e: EVENT) : BOOLEAN ;
    
    
    (*
       ReArmEvent - removes an event, e, from the event queue. A new time
                    is given to this event and it is then re-inserted onto the
                    event queue in the correct place.
                    TRUE is returned if this occurred
                    FALSE is returned if the event was not found.
    *)
    
.. index::
   ReArmEvent
.. code-block:: modula2
    PROCEDURE ReArmEvent (e: EVENT; t: CARDINAL) : BOOLEAN ;
    
    
    END TimerHandler.


@c ------------------------------------------------------------
@c @node M2 ISO Libraries, , PIM coroutine support, Libraries
M2 ISO Libraries
================

@menu
* gm2-libs-iso/COROUTINES::COROUTINES.def
* gm2-libs-iso/ChanConsts::ChanConsts.def
* gm2-libs-iso/CharClass::CharClass.def
* gm2-libs-iso/ClientSocket::ClientSocket.def
* gm2-libs-iso/ComplexMath::ComplexMath.def
* gm2-libs-iso/ConvStringLong::ConvStringLong.def
* gm2-libs-iso/ConvStringReal::ConvStringReal.def
* gm2-libs-iso/ConvTypes::ConvTypes.def
* gm2-libs-iso/EXCEPTIONS::EXCEPTIONS.def
* gm2-libs-iso/ErrnoCategory::ErrnoCategory.def
* gm2-libs-iso/GeneralUserExceptions::GeneralUserExceptions.def
* gm2-libs-iso/IOChan::IOChan.def
* gm2-libs-iso/IOConsts::IOConsts.def
* gm2-libs-iso/IOLink::IOLink.def
* gm2-libs-iso/IOResult::IOResult.def
* gm2-libs-iso/LongComplexMath::LongComplexMath.def
* gm2-libs-iso/LongConv::LongConv.def
* gm2-libs-iso/LongIO::LongIO.def
* gm2-libs-iso/LongMath::LongMath.def
* gm2-libs-iso/LongStr::LongStr.def
* gm2-libs-iso/LongWholeIO::LongWholeIO.def
* gm2-libs-iso/LowLong::LowLong.def
* gm2-libs-iso/LowReal::LowReal.def
* gm2-libs-iso/LowShort::LowShort.def
* gm2-libs-iso/M2EXCEPTION::M2EXCEPTION.def
* gm2-libs-iso/M2RTS::M2RTS.def
* gm2-libs-iso/MemStream::MemStream.def
* gm2-libs-iso/Preemptive::Preemptive.def
* gm2-libs-iso/Processes::Processes.def
* gm2-libs-iso/ProgramArgs::ProgramArgs.def
* gm2-libs-iso/RTco::RTco.def
* gm2-libs-iso/RTdata::RTdata.def
* gm2-libs-iso/RTentity::RTentity.def
* gm2-libs-iso/RTfio::RTfio.def
* gm2-libs-iso/RTgen::RTgen.def
* gm2-libs-iso/RTgenif::RTgenif.def
* gm2-libs-iso/RTio::RTio.def
* gm2-libs-iso/RandomNumber::RandomNumber.def
* gm2-libs-iso/RawIO::RawIO.def
* gm2-libs-iso/RealConv::RealConv.def
* gm2-libs-iso/RealIO::RealIO.def
* gm2-libs-iso/RealMath::RealMath.def
* gm2-libs-iso/RealStr::RealStr.def
* gm2-libs-iso/RndFile::RndFile.def
* gm2-libs-iso/SIOResult::SIOResult.def
* gm2-libs-iso/SLongIO::SLongIO.def
* gm2-libs-iso/SLongWholeIO::SLongWholeIO.def
* gm2-libs-iso/SRawIO::SRawIO.def
* gm2-libs-iso/SRealIO::SRealIO.def
* gm2-libs-iso/SShortIO::SShortIO.def
* gm2-libs-iso/SShortWholeIO::SShortWholeIO.def
* gm2-libs-iso/STextIO::STextIO.def
* gm2-libs-iso/SWholeIO::SWholeIO.def
* gm2-libs-iso/SYSTEM::SYSTEM.def
* gm2-libs-iso/Semaphores::Semaphores.def
* gm2-libs-iso/SeqFile::SeqFile.def
* gm2-libs-iso/ShortComplexMath::ShortComplexMath.def
* gm2-libs-iso/ShortIO::ShortIO.def
* gm2-libs-iso/ShortWholeIO::ShortWholeIO.def
* gm2-libs-iso/SimpleCipher::SimpleCipher.def
* gm2-libs-iso/StdChans::StdChans.def
* gm2-libs-iso/Storage::Storage.def
* gm2-libs-iso/StreamFile::StreamFile.def
* gm2-libs-iso/StringChan::StringChan.def
* gm2-libs-iso/Strings::Strings.def
* gm2-libs-iso/SysClock::SysClock.def
* gm2-libs-iso/TERMINATION::TERMINATION.def
* gm2-libs-iso/TermFile::TermFile.def
* gm2-libs-iso/TextIO::TextIO.def
* gm2-libs-iso/WholeConv::WholeConv.def
* gm2-libs-iso/WholeIO::WholeIO.def
* gm2-libs-iso/WholeStr::WholeStr.def
* gm2-libs-iso/wrapsock::wrapsock.def
* gm2-libs-iso/wraptime::wraptime.def
@end menu

@c @node gm2-libs-iso/COROUTINES, gm2-libs-iso/ChanConsts, , M2 ISO Libraries
gm2-libs-iso/COROUTINES
-----------------------

.. code-block:: modula2
    DEFINITION MODULE COROUTINES;

(* Facilities for coroutines and the handling of interrupts *)
    
    IMPORT SYSTEM ;
    
    
    CONST
.. index::
   pair: UnassignedPriority; (const)
.. code-block:: modula2
      UnassignedPriority = 0 ;
    
    TYPE
.. index::
   pair: COROUTINE; (type)
.. code-block:: modula2
      COROUTINE ; (* Values of this type are created dynamically by NEWCOROUTINE
                     and identify the coroutine in subsequent operations *)
.. index::
   pair: INTERRUPTSOURCE; (type)
.. code-block:: modula2
      INTERRUPTSOURCE = CARDINAL ;
.. index::
   pair: PROTECTION; (type)
.. code-block:: modula2
      PROTECTION = [UnassignedPriority..7] ;
    
    
.. index::
   NEWCOROUTINE
.. code-block:: modula2
    PROCEDURE NEWCOROUTINE (procBody: PROC;
                            workspace: SYSTEM.ADDRESS;
                            size: CARDINAL;
                            VAR cr: COROUTINE;
                            [initProtection: PROTECTION = UnassignedPriority]);
      (* Creates a new coroutine whose body is given by procBody, and
         returns the identity of the coroutine in cr. workspace is a
         pointer to the work space allocated to the coroutine; size
         specifies the size of this workspace in terms of SYSTEM.LOC.
    
         The optarg, initProtection, may contain a single parameter which
         specifies the initial protection level of the coroutine.
      *)
    
.. index::
   TRANSFER
.. code-block:: modula2
    PROCEDURE TRANSFER (VAR from: COROUTINE; to: COROUTINE);
      (* Returns the identity of the calling coroutine in from, and
         transfers control to the coroutine specified by to.
      *)
    
.. index::
   IOTRANSFER
.. code-block:: modula2
    PROCEDURE IOTRANSFER (VAR from: COROUTINE; to: COROUTINE);
      (* Returns the identity of the calling coroutine in from and
         transfers control to the coroutine specified by to.  On
         occurrence of an interrupt, associated with the caller, control
         is transferred back to the caller, and the identity of the
         interrupted coroutine is returned in from.  The calling coroutine
         must be associated with a source of interrupts.
      *)
    
.. index::
   ATTACH
.. code-block:: modula2
    PROCEDURE ATTACH (source: INTERRUPTSOURCE);
      (* Associates the specified source of interrupts with the calling
         coroutine. *)
    
.. index::
   DETACH
.. code-block:: modula2
    PROCEDURE DETACH (source: INTERRUPTSOURCE);
      (* Dissociates the specified source of interrupts from the calling
         coroutine. *)
    
.. index::
   IsATTACHED
.. code-block:: modula2
    PROCEDURE IsATTACHED (source: INTERRUPTSOURCE): BOOLEAN;
      (* Returns TRUE if and only if the specified source of interrupts is
         currently associated with a coroutine; otherwise returns FALSE.
      *)
    
.. index::
   HANDLER
.. code-block:: modula2
    PROCEDURE HANDLER (source: INTERRUPTSOURCE): COROUTINE;
      (* Returns the coroutine, if any, that is associated with the source
         of interrupts. The result is undefined if IsATTACHED(source) =
         FALSE.
      *)
    
.. index::
   CURRENT
.. code-block:: modula2
    PROCEDURE CURRENT (): COROUTINE;
      (* Returns the identity of the calling coroutine. *)
    
.. index::
   LISTEN
.. code-block:: modula2
    PROCEDURE LISTEN (p: PROTECTION);
      (* Momentarily changes the protection of the calling coroutine to
         p. *)
    
.. index::
   PROT
.. code-block:: modula2
    PROCEDURE PROT (): PROTECTION;
      (* Returns the protection of the calling coroutine. *)
    
    
    (*
       TurnInterrupts - switches processor interrupts to the protection
                        level, to.  It returns the old value.
    *)
    
.. index::
   TurnInterrupts
.. code-block:: modula2
    PROCEDURE TurnInterrupts (to: PROTECTION) : PROTECTION ;
    
    
    (*
       ListenLoop - should be called instead of users writing:
    
                    LOOP
                       LISTEN
                    END
    
                    It performs the same function but yields
                    control back to the underlying operating system.
                    It also checks for deadlock.
                    Note that this function does return when an interrupt occurs.
                    (File descriptor becomes ready or time event expires).
    *)
    
.. index::
   ListenLoop
.. code-block:: modula2
    PROCEDURE ListenLoop ;
    
    
    END COROUTINES.

@c @node gm2-libs-iso/ChanConsts, gm2-libs-iso/CharClass, gm2-libs-iso/COROUTINES, M2 ISO Libraries
gm2-libs-iso/ChanConsts
-----------------------

.. code-block:: modula2
    DEFINITION MODULE ChanConsts;

  (* Common types and values for channel open requests and results *)
    
    TYPE
.. index::
   pair: ChanFlags; (type)
.. code-block:: modula2
      ChanFlags =        (* Request flags possibly given when a channel is opened *)
      ( readFlag,        (* input operations are requested/available *)
        writeFlag,       (* output operations are requested/available *)
        oldFlag,         (* a file may/must/did exist before the channel is opened *)
        textFlag,        (* text operations are requested/available *)
        rawFlag,         (* raw operations are requested/available *)
        interactiveFlag, (* interactive use is requested/applies *)
        echoFlag         (* echoing by interactive device on removal of characters from input
                            stream requested/applies *)
      );
    
.. index::
   pair: FlagSet; (type)
.. code-block:: modula2
      FlagSet = SET OF ChanFlags;
    
      (* Singleton values of FlagSet, to allow for example, read + write *)
    
    CONST
.. index::
   pair: read; (const)
.. code-block:: modula2
      read = FlagSet{readFlag};   (* input operations are requested/available *)
.. index::
   pair: write; (const)
.. code-block:: modula2
      write = FlagSet{writeFlag}; (* output operations are requested/available *)
.. index::
   pair: old; (const)
.. code-block:: modula2
      old = FlagSet{oldFlag};     (* a file may/must/did exist before the channel is opened *)
.. index::
   pair: text; (const)
.. code-block:: modula2
      text = FlagSet{textFlag};   (* text operations are requested/available *)
.. index::
   pair: raw; (const)
.. code-block:: modula2
      raw = FlagSet{rawFlag};     (* raw operations are requested/available *)
.. index::
   pair: interactive; (const)
.. code-block:: modula2
      interactive = FlagSet{interactiveFlag}; (* interactive use is requested/applies *)
.. index::
   pair: echo; (const)
.. code-block:: modula2
      echo = FlagSet{echoFlag};   (* echoing by interactive device on removal of characters from
                                     input stream requested/applies *)
    
    TYPE
.. index::
   pair: OpenResults; (type)
.. code-block:: modula2
      OpenResults =        (* Possible results of open requests *)
        (opened,           (* the open succeeded as requested *)
         wrongNameFormat,  (* given name is in the wrong format for the implementation *)
         wrongFlags,       (* given flags include a value that does not apply to the device *)
         tooManyOpen,      (* this device cannot support any more open channels *)
         outOfChans,       (* no more channels can be allocated *)
         wrongPermissions, (* file or directory permissions do not allow request *)
         noRoomOnDevice,   (* storage limits on the device prevent the open *)
         noSuchFile,       (* a needed file does not exist *)
         fileExists,       (* a file of the given name already exists when a new one is required *)
         wrongFileType,    (* the file is of the wrong type to support the required operations *)
         noTextOperations, (* text operations have been requested, but are not supported *)
         noRawOperations,  (* raw operations have been requested, but are not supported *)
         noMixedOperations,(* text and raw operations have been requested, but they
                              are not supported in combination *)
         alreadyOpen,      (* the source/destination is already open for operations not supported
                              in combination with the requested operations *)
         otherProblem      (* open failed for some other reason *)
        );
    
    END ChanConsts.
    

@c @node gm2-libs-iso/CharClass, gm2-libs-iso/ClientSocket, gm2-libs-iso/ChanConsts, M2 ISO Libraries
gm2-libs-iso/CharClass
----------------------

.. code-block:: modula2
    DEFINITION MODULE CharClass;

  (* Classification of values of the type CHAR *)
    
.. index::
   IsNumeric
.. code-block:: modula2
    PROCEDURE IsNumeric (ch: CHAR): BOOLEAN;
      (* Returns TRUE if and only if ch is classified as a numeric character *)
    
.. index::
   IsLetter
.. code-block:: modula2
    PROCEDURE IsLetter (ch: CHAR): BOOLEAN;
      (* Returns TRUE if and only if ch is classified as a letter *)
    
.. index::
   IsUpper
.. code-block:: modula2
    PROCEDURE IsUpper (ch: CHAR): BOOLEAN;
      (* Returns TRUE if and only if ch is classified as an upper case letter *)
    
.. index::
   IsLower
.. code-block:: modula2
    PROCEDURE IsLower (ch: CHAR): BOOLEAN;
      (* Returns TRUE if and only if ch is classified as a lower case letter *)
    
.. index::
   IsControl
.. code-block:: modula2
    PROCEDURE IsControl (ch: CHAR): BOOLEAN;
      (* Returns TRUE if and only if ch represents a control function *)
    
.. index::
   IsWhiteSpace
.. code-block:: modula2
    PROCEDURE IsWhiteSpace (ch: CHAR): BOOLEAN;
      (* Returns TRUE if and only if ch represents a space character or a format effector *)
    
    END CharClass.
    

@c @node gm2-libs-iso/ClientSocket, gm2-libs-iso/ComplexMath, gm2-libs-iso/CharClass, M2 ISO Libraries
gm2-libs-iso/ClientSocket
-------------------------

.. code-block:: modula2
    DEFINITION MODULE ClientSocket ;

    FROM IOChan IMPORT ChanId ;
    FROM ChanConsts IMPORT FlagSet, OpenResults ;
    
    
    (*
       OpenSocket - opens a TCP client connection to host:port.
    *)
    
.. index::
   OpenSocket
.. code-block:: modula2
    PROCEDURE OpenSocket (VAR cid: ChanId;
                          host: ARRAY OF CHAR; port: CARDINAL;
                          f: FlagSet; VAR res: OpenResults) ;
    
    (*
       Close - if the channel identified by cid is not open to
               a socket stream, the exception wrongDevice is
               raised; otherwise closes the channel, and assigns
               the value identifying the invalid channel to cid.
    *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (VAR cid: ChanId) ;
    
    
    (*
       IsSocket - tests if the channel identified by cid is open as
                  a client socket stream.
    *)
    
.. index::
   IsSocket
.. code-block:: modula2
    PROCEDURE IsSocket (cid: ChanId) : BOOLEAN ;
    
    
    END ClientSocket.

@c @node gm2-libs-iso/ComplexMath, gm2-libs-iso/ConvStringLong, gm2-libs-iso/ClientSocket, M2 ISO Libraries
gm2-libs-iso/ComplexMath
------------------------

.. code-block:: modula2
    DEFINITION MODULE ComplexMath;

  (* Mathematical functions for the type COMPLEX *)
    
    CONST
.. index::
   pair: i; (const)
.. code-block:: modula2
      i =    CMPLX (0.0, 1.0);
.. index::
   pair: one; (const)
.. code-block:: modula2
      one =  CMPLX (1.0, 0.0);
.. index::
   pair: zero; (const)
.. code-block:: modula2
      zero = CMPLX (0.0, 0.0);
    
.. index::
   abs
.. code-block:: modula2
    PROCEDURE __BUILTIN__ abs (z: COMPLEX): REAL;
      (* Returns the length of z *)
    
.. index::
   arg
.. code-block:: modula2
    PROCEDURE __BUILTIN__ arg (z: COMPLEX): REAL;
      (* Returns the angle that z subtends to the positive real axis *)
    
.. index::
   conj
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conj (z: COMPLEX): COMPLEX;
      (* Returns the complex conjugate of z *)
    
.. index::
   power
.. code-block:: modula2
    PROCEDURE __BUILTIN__ power (base: COMPLEX; exponent: REAL): COMPLEX;
      (* Returns the value of the number base raised to the power exponent *)
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (z: COMPLEX): COMPLEX;
      (* Returns the principal square root of z *)
    
.. index::
   exp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp (z: COMPLEX): COMPLEX;
      (* Returns the complex exponential of z *)
    
.. index::
   ln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ln (z: COMPLEX): COMPLEX;
      (* Returns the principal value of the natural logarithm of z *)
    
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (z: COMPLEX): COMPLEX;
      (* Returns the sine of z *)
    
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (z: COMPLEX): COMPLEX;
      (* Returns the cosine of z *)
    
.. index::
   tan
.. code-block:: modula2
    PROCEDURE __BUILTIN__ tan (z: COMPLEX): COMPLEX;
      (* Returns the tangent of z *)
    
.. index::
   arcsin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ arcsin (z: COMPLEX): COMPLEX;
      (* Returns the arcsine of z *)
    
.. index::
   arccos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ arccos (z: COMPLEX): COMPLEX;
      (* Returns the arccosine of z *)
    
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE __BUILTIN__ arctan (z: COMPLEX): COMPLEX;
      (* Returns the arctangent of z *)
    
.. index::
   polarToComplex
.. code-block:: modula2
    PROCEDURE polarToComplex (abs, arg: REAL): COMPLEX;
      (* Returns the complex number with the specified polar coordinates *)
    
.. index::
   scalarMult
.. code-block:: modula2
    PROCEDURE scalarMult (scalar: REAL; z: COMPLEX): COMPLEX;
      (* Returns the scalar product of scalar with z *)
    
.. index::
   IsCMathException
.. code-block:: modula2
    PROCEDURE IsCMathException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception in a
         routine from this module; otherwise returns FALSE.
      *)
    
    END ComplexMath.
    

@c @node gm2-libs-iso/ConvStringLong, gm2-libs-iso/ConvStringReal, gm2-libs-iso/ComplexMath, M2 ISO Libraries
gm2-libs-iso/ConvStringLong
---------------------------

.. code-block:: modula2
    DEFINITION MODULE ConvStringLong ;

    FROM DynamicStrings IMPORT String ;
    
    
    (*
       RealToFloatString - converts a real with, sigFigs, into a string
                           and returns the result as a string.
    *)
    
.. index::
   RealToFloatString
.. code-block:: modula2
    PROCEDURE RealToFloatString (real: LONGREAL; sigFigs: CARDINAL) : String ;
    
    
    (*
       RealToEngString - converts the value of real to floating-point
                         string form, with sigFigs significant figures.
                         The number is scaled with one to three digits
                         in the whole number part and with an exponent
                         that is a multiple of three.
    *)
    
.. index::
   RealToEngString
.. code-block:: modula2
    PROCEDURE RealToEngString (real: LONGREAL; sigFigs: CARDINAL) : String ;
    
    
    (*
       RealToFixedString - returns the number of characters in the fixed-point
                           string representation of real rounded to the given
                           place relative to the decimal point.
    *)
    
.. index::
   RealToFixedString
.. code-block:: modula2
    PROCEDURE RealToFixedString (real: LONGREAL; place: INTEGER) : String ;
    
    
    END ConvStringLong.

@c @node gm2-libs-iso/ConvStringReal, gm2-libs-iso/ConvTypes, gm2-libs-iso/ConvStringLong, M2 ISO Libraries
gm2-libs-iso/ConvStringReal
---------------------------

.. code-block:: modula2
    DEFINITION MODULE ConvStringReal ;

    FROM DynamicStrings IMPORT String ;
    
    
    (*
       RealToFloatString - converts a real with, sigFigs, into a string
                           and returns the result as a string.
    *)
    
.. index::
   RealToFloatString
.. code-block:: modula2
    PROCEDURE RealToFloatString (real: REAL; sigFigs: CARDINAL) : String ;
    
    
    (*
       RealToEngString - converts the value of real to floating-point
                         string form, with sigFigs significant figures.
                         The number is scaled with one to three digits
                         in the whole number part and with an exponent
                         that is a multiple of three.
    *)
    
.. index::
   RealToEngString
.. code-block:: modula2
    PROCEDURE RealToEngString (real: REAL; sigFigs: CARDINAL) : String ;
    
    
    (*
       RealToFixedString - returns the number of characters in the fixed-point
                           string representation of real rounded to the given
                           place relative to the decimal point.
    *)
    
.. index::
   RealToFixedString
.. code-block:: modula2
    PROCEDURE RealToFixedString (real: REAL; place: INTEGER) : String ;
    
    
    END ConvStringReal.

@c @node gm2-libs-iso/ConvTypes, gm2-libs-iso/EXCEPTIONS, gm2-libs-iso/ConvStringReal, M2 ISO Libraries
gm2-libs-iso/ConvTypes
----------------------

.. code-block:: modula2
    DEFINITION MODULE ConvTypes;

  (* Common types used in the string conversion modules *)
    
    TYPE
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
      ConvResults =     (* Values of this type are used to express the format of a string *)
      (
        strAllRight,    (* the string format is correct for the corresponding conversion *)
        strOutOfRange,  (* the string is well-formed but the value cannot be represented *)
        strWrongFormat, (* the string is in the wrong format for the conversion *)
        strEmpty        (* the given string is empty *)
      );
    
.. index::
   pair: ScanClass; (type)
.. code-block:: modula2
      ScanClass =  (* Values of this type are used to classify input to finite state scanners *)
      (
        padding,   (* a leading or padding character at this point in the scan - ignore it *)
        valid,     (* a valid character at this point in the scan - accept it *)
        invalid,   (* an invalid character at this point in the scan - reject it *)
        terminator (* a terminating character at this point in the scan (not part of token) *)
      );
    
.. index::
   pair: ScanState; (type)
.. code-block:: modula2
      ScanState =  (* The type of lexical scanning control procedures *)
        PROCEDURE (CHAR, VAR ScanClass, VAR ScanState);
    
    END ConvTypes.
    

@c @node gm2-libs-iso/EXCEPTIONS, gm2-libs-iso/ErrnoCategory, gm2-libs-iso/ConvTypes, M2 ISO Libraries
gm2-libs-iso/EXCEPTIONS
-----------------------

.. code-block:: modula2
    DEFINITION MODULE EXCEPTIONS;

(* Provides facilities for raising user exceptions
   and for making enquiries concerning the current execution state.
*)
    
    TYPE
      ExceptionSource;   (* values of this type are used within library
                            modules to identify the source of raised
                            exceptions *)
.. index::
   pair: ExceptionNumber; (type)
.. code-block:: modula2
      ExceptionNumber = CARDINAL;
    
.. index::
   AllocateSource
.. code-block:: modula2
    PROCEDURE AllocateSource(VAR newSource: ExceptionSource);
      (* Allocates a unique value of type ExceptionSource *)
    
.. index::
   RAISE
.. code-block:: modula2
    PROCEDURE RAISE (source: ExceptionSource;
                     number: ExceptionNumber; message: ARRAY OF CHAR);
      (* Associates the given values of source, number and message with
         the current context and raises an exception.
      *)
    
.. index::
   CurrentNumber
.. code-block:: modula2
    PROCEDURE CurrentNumber (source: ExceptionSource): ExceptionNumber;
      (* If the current coroutine is in the exceptional execution state
         because of the raising of an exception from source, returns
         the corresponding number, and otherwise raises an exception.
      *)
    
.. index::
   GetMessage
.. code-block:: modula2
    PROCEDURE GetMessage (VAR text: ARRAY OF CHAR);
      (* If the current coroutine is in the exceptional execution state,
         returns the possibly truncated string associated with the
         current context.  Otherwise, in normal execution state,
         returns the empty string.
      *)
    
.. index::
   IsCurrentSource
.. code-block:: modula2
    PROCEDURE IsCurrentSource (source: ExceptionSource): BOOLEAN;
      (* If the current coroutine is in the exceptional execution state
         because of the raising of an exception from source, returns
         TRUE, and otherwise returns FALSE.
      *)
    
.. index::
   IsExceptionalExecution
.. code-block:: modula2
    PROCEDURE IsExceptionalExecution (): BOOLEAN;
      (* If the current coroutine is in the exceptional execution state
         because of the raising of an exception, returns TRUE, and
         otherwise returns FALSE.
      *)
    
    END EXCEPTIONS.

@c @node gm2-libs-iso/ErrnoCategory, gm2-libs-iso/GeneralUserExceptions, gm2-libs-iso/EXCEPTIONS, M2 ISO Libraries
gm2-libs-iso/ErrnoCategory
--------------------------

.. code-block:: modula2
    DEFINITION MODULE ErrnoCategory ;

(*
   provides an interface to errno (if the system
   supports it) which determines whether the current
   errno is a hard or soft error.  These distinctions
   are needed by the ISO Modula-2 libraries.  Not all
   errno values are tested, only those which could be
   related to a device.
*)
    
    IMPORT ChanConsts ;
    
    
    (*
       IsErrnoHard - returns TRUE if the value of errno is associated with
                     a hard device error.
    *)
    
.. index::
   IsErrnoHard
.. code-block:: modula2
    PROCEDURE IsErrnoHard (e: INTEGER) : BOOLEAN ;
    
    
    (*
       IsErrnoSoft - returns TRUE if the value of errno is associated with
                     a soft device error.
    *)
    
.. index::
   IsErrnoSoft
.. code-block:: modula2
    PROCEDURE IsErrnoSoft (e: INTEGER) : BOOLEAN ;
    
    
    (*
       UnAvailable - returns TRUE if the value of errno indicates that
                     the resource or device is unavailable for some
                     reason.
    *)
    
.. index::
   UnAvailable
.. code-block:: modula2
    PROCEDURE UnAvailable (e: INTEGER) : BOOLEAN ;
    
    
    (*
       GetOpenResults - maps errno onto the ISO Modula-2 enumerated
                        type, OpenResults.
    *)
    
.. index::
   GetOpenResults
.. code-block:: modula2
    PROCEDURE GetOpenResults (e: INTEGER) : ChanConsts.OpenResults ;
    
    
    END ErrnoCategory.

@c @node gm2-libs-iso/GeneralUserExceptions, gm2-libs-iso/IOChan, gm2-libs-iso/ErrnoCategory, M2 ISO Libraries
gm2-libs-iso/GeneralUserExceptions
----------------------------------

.. code-block:: modula2
    DEFINITION MODULE GeneralUserExceptions;

(* Provides facilities for general user-defined exceptions *)
    
    TYPE
.. index::
   pair: GeneralExceptions; (type)
.. code-block:: modula2
      GeneralExceptions = (problem, disaster);
    
.. index::
   RaiseGeneralException
.. code-block:: modula2
    PROCEDURE RaiseGeneralException (exception: GeneralExceptions;
                                     text: ARRAY OF CHAR);
      (* Raises exception using text as the associated message *)
    
.. index::
   IsGeneralException
.. code-block:: modula2
    PROCEDURE IsGeneralException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception from
         GeneralExceptions; otherwise returns FALSE.
      *)
    
.. index::
   GeneralException
.. code-block:: modula2
    PROCEDURE GeneralException(): GeneralExceptions;
      (* If the current coroutine is in the exceptional execution
         state because of the raising of an exception from
         GeneralExceptions, returns the corresponding enumeration value,
         and otherwise raises an exception.
      *)
    
    END GeneralUserExceptions.

@c @node gm2-libs-iso/IOChan, gm2-libs-iso/IOConsts, gm2-libs-iso/GeneralUserExceptions, M2 ISO Libraries
gm2-libs-iso/IOChan
-------------------

.. code-block:: modula2
    DEFINITION MODULE IOChan;

  (* Types and procedures forming the interface to channels for
     device-independent data transfer modules
  *)
    
    IMPORT IOConsts, ChanConsts, SYSTEM;
    
    TYPE
      ChanId; (* Values of this type are used to identify channels *)
    
      (* There is one pre-defined value identifying an invalid channel
         on which no data transfer operations are available.  It may
         be used to initialize variables of type ChanId.
      *)
    
.. index::
   InvalidChan
.. code-block:: modula2
    PROCEDURE InvalidChan (): ChanId;
      (* Returns the value identifying the invalid channel. *)
    
      (* For each of the following operations, if the device supports
         the operation on the channel, the behaviour of the procedure
         conforms with the description below.  The full behaviour is
         defined for each device module.  If the device does not
         support the operation on the channel, the behaviour of the
         procedure is to raise the exception notAvailable.
      *)
    
      (* Text operations - these perform any required translation between the
         internal and external representation of text.
      *)
    
.. index::
   Look
.. code-block:: modula2
    PROCEDURE Look (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
      (* If there is a character as the next item in the input stream
         cid, assigns its value to ch without removing it from the stream;
         otherwise the value of ch is not defined.  res (and the stored
         read result) are set to the value allRight, endOfLine, or endOfInput.
      *)
    
.. index::
   Skip
.. code-block:: modula2
    PROCEDURE Skip (cid: ChanId);
      (* If the input stream cid has ended, the exception skipAtEnd
         is raised; otherwise the next character or line mark in cid is
         removed, and the stored read result is set to the value
         allRight.
      *)
    
.. index::
   SkipLook
.. code-block:: modula2
    PROCEDURE SkipLook (cid: ChanId; VAR ch: CHAR; VAR res: IOConsts.ReadResults);
      (* If the input stream cid has ended, the exception skipAtEnd is
         raised; otherwise the next character or line mark in cid is
         removed.  If there is a character as the next item in cid
         stream, assigns its value to ch without removing it from the
         stream.  Otherwise, the value of ch is not defined.  res
         (and the stored read result) are set to the value allRight,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteLn
.. code-block:: modula2
    PROCEDURE WriteLn (cid: ChanId);
      (* Writes a line mark over the channel cid. *)
    
.. index::
   TextRead
.. code-block:: modula2
    PROCEDURE TextRead (cid: ChanId; to: SYSTEM.ADDRESS; maxChars: CARDINAL;
                        VAR charsRead: CARDINAL);
      (* Reads at most maxChars characters from the current line in cid,
         and assigns corresponding values to successive components of
         an ARRAY OF CHAR variable for which the address of the first
         component is to. The number of characters read is assigned to charsRead.
         The stored read result is set to allRight, endOfLine, or endOfInput.
      *)
    
.. index::
   TextWrite
.. code-block:: modula2
    PROCEDURE TextWrite (cid: ChanId; from: SYSTEM.ADDRESS;
                         charsToWrite: CARDINAL);
      (* Writes a number of characters given by the value of charsToWrite,
         from successive components of an ARRAY OF CHAR variable for which
         the address of the first component is from, to the channel cid.
      *)
    
      (* Direct raw operations  - these do not effect translation between
         the internal and external representation of data
      *)
    
.. index::
   RawRead
.. code-block:: modula2
    PROCEDURE RawRead (cid: ChanId; to: SYSTEM.ADDRESS; maxLocs: CARDINAL;
                       VAR locsRead: CARDINAL);
      (* Reads at most maxLocs items from cid, and assigns corresponding
         values to successive components of an ARRAY OF LOC variable for
         which the address of the first component is to. The number of
         characters read is assigned to charsRead. The stored read result
         is set to the value allRight, or endOfInput.
      *)
    
.. index::
   RawWrite
.. code-block:: modula2
    PROCEDURE RawWrite (cid: ChanId; from: SYSTEM.ADDRESS; locsToWrite: CARDINAL);
      (* Writes a number of items given by the value of charsToWrite,
         from successive components of an ARRAY OF LOC variable for
         which the address of the first component is from, to the channel cid.
      *)
    
      (* Common operations *)
    
.. index::
   GetName
.. code-block:: modula2
    PROCEDURE GetName (cid: ChanId; VAR s: ARRAY OF CHAR);
      (* Copies to s a name associated with the channel cid, possibly truncated
         (depending on the capacity of s).
      *)
    
.. index::
   Reset
.. code-block:: modula2
    PROCEDURE Reset (cid: ChanId);
      (* Resets the channel cid to a state defined by the device module. *)
    
.. index::
   Flush
.. code-block:: modula2
    PROCEDURE Flush (cid: ChanId);
      (* Flushes any data buffered by the device module out to the channel cid. *)
    
      (* Access to read results *)
    
.. index::
   SetReadResult
.. code-block:: modula2
    PROCEDURE SetReadResult (cid: ChanId; res: IOConsts.ReadResults);
      (* Sets the read result value for the channel cid to the value res. *)
    
.. index::
   ReadResult
.. code-block:: modula2
    PROCEDURE ReadResult (cid: ChanId): IOConsts.ReadResults;
      (* Returns the stored read result value for the channel cid.
         (This is initially the value notKnown).
      *)
    
      (* Users can discover which flags actually apply to a channel *)
    
.. index::
   CurrentFlags
.. code-block:: modula2
    PROCEDURE CurrentFlags (cid: ChanId): ChanConsts.FlagSet;
      (* Returns the set of flags that currently apply to the channel cid. *)
    
      (* The following exceptions are defined for this module and its clients *)
    
    TYPE
.. index::
   pair: ChanExceptions; (type)
.. code-block:: modula2
      ChanExceptions =
        (wrongDevice,      (* device specific operation on wrong device *)
         notAvailable,     (* operation attempted that is not available on that
                              channel *)
         skipAtEnd,        (* attempt to skip data from a stream that has ended *)
         softDeviceError,  (* device specific recoverable error *)
         hardDeviceError,  (* device specific non-recoverable error *)
         textParseError,   (* input data does not correspond to a character or
                              line mark - optional detection *)
         notAChannel       (* given value does not identify a channel -
                              optional detection *)
        );
    
.. index::
   IsChanException
.. code-block:: modula2
    PROCEDURE IsChanException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception from
         ChanExceptions; otherwise returns FALSE.
      *)
    
.. index::
   ChanException
.. code-block:: modula2
    PROCEDURE ChanException (): ChanExceptions;
      (* If the current coroutine is in the exceptional execution state
         because of the raising of an exception from ChanExceptions,
         returns the corresponding enumeration value, and otherwise
         raises an exception.
      *)
    
      (* When a device procedure detects a device error, it raises the
         exception softDeviceError or hardDeviceError.  If these
         exceptions are handled, the following facilities may be
         used to discover an implementation-defined error number for
         the channel.
      *)
    
    TYPE
.. index::
   pair: DeviceErrNum; (type)
.. code-block:: modula2
      DeviceErrNum = INTEGER;
    
.. index::
   DeviceError
.. code-block:: modula2
    PROCEDURE DeviceError (cid: ChanId): DeviceErrNum;
      (* If a device error exception has been raised for the channel cid,
         returns the error number stored by the device module.
      *)
    
    END IOChan.

@c @node gm2-libs-iso/IOConsts, gm2-libs-iso/IOLink, gm2-libs-iso/IOChan, M2 ISO Libraries
gm2-libs-iso/IOConsts
---------------------

.. code-block:: modula2
    DEFINITION MODULE IOConsts;

  (* Types and constants for input/output modules *)
    
    TYPE
.. index::
   pair: ReadResults; (type)
.. code-block:: modula2
      ReadResults =  (* This type is used to classify the result of an input operation *)
      (
        notKnown,    (* no read result is set *)
        allRight,    (* data is as expected or as required *)
        outOfRange,  (* data cannot be represented *)
        wrongFormat, (* data not in expected format *)
        endOfLine,   (* end of line seen before expected data *)
        endOfInput   (* end of input seen before expected data *)
      );
    
    END IOConsts.
    

@c @node gm2-libs-iso/IOLink, gm2-libs-iso/IOResult, gm2-libs-iso/IOConsts, M2 ISO Libraries
gm2-libs-iso/IOLink
-------------------

.. code-block:: modula2
    DEFINITION MODULE IOLink;

(* Types and procedures for the standard implementation of channels *)
    
    IMPORT IOChan, IOConsts, ChanConsts, SYSTEM;
    
    TYPE
      DeviceId;
        (* Values of this type are used to identify new device modules,
           and are normally obtained by them during their initialization.
        *)
    
.. index::
   AllocateDeviceId
.. code-block:: modula2
    PROCEDURE AllocateDeviceId (VAR did: DeviceId);
      (* Allocates a unique value of type DeviceId, and assigns this
         value to did. *)
    
.. index::
   MakeChan
.. code-block:: modula2
    PROCEDURE MakeChan (did: DeviceId; VAR cid: IOChan.ChanId);
      (* Attempts to make a new channel for the device module identified
         by did. If no more channels can be made, the identity of
         the invalid channel is assigned to cid.  Otherwise, the identity
         of a new channel is assigned to cid.
      *)
    
.. index::
   UnMakeChan
.. code-block:: modula2
    PROCEDURE UnMakeChan (did: DeviceId; VAR cid: IOChan.ChanId);
      (* If the device module identified by did is not the module that
         made the channel identified by cid, the exception wrongDevice is
         raised; otherwise the channel is deallocated, and the value
         identifying the invalid channel is assigned to cid.
      *)
    
    TYPE
.. index::
   pair: DeviceTablePtr; (type)
.. code-block:: modula2
      DeviceTablePtr = POINTER TO DeviceTable;
        (* Values of this type are used to refer to device tables *)
    
    TYPE
.. index::
   pair: LookProc; (type)
.. code-block:: modula2
      LookProc      = PROCEDURE (DeviceTablePtr, VAR CHAR, VAR IOConsts.ReadResults) ;
.. index::
   pair: SkipProc; (type)
.. code-block:: modula2
      SkipProc      = PROCEDURE (DeviceTablePtr) ;
.. index::
   pair: SkipLookProc; (type)
.. code-block:: modula2
      SkipLookProc  = PROCEDURE (DeviceTablePtr, VAR CHAR, VAR IOConsts.ReadResults) ;
.. index::
   pair: WriteLnProc; (type)
.. code-block:: modula2
      WriteLnProc   = PROCEDURE (DeviceTablePtr) ;
.. index::
   pair: TextReadProc; (type)
.. code-block:: modula2
      TextReadProc  = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL, VAR CARDINAL) ;
.. index::
   pair: TextWriteProc; (type)
.. code-block:: modula2
      TextWriteProc = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL) ;
.. index::
   pair: RawReadProc; (type)
.. code-block:: modula2
      RawReadProc   = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL, VAR CARDINAL) ;
.. index::
   pair: RawWriteProc; (type)
.. code-block:: modula2
      RawWriteProc  = PROCEDURE (DeviceTablePtr, SYSTEM.ADDRESS, CARDINAL) ;
.. index::
   pair: GetNameProc; (type)
.. code-block:: modula2
      GetNameProc   = PROCEDURE (DeviceTablePtr, VAR ARRAY OF CHAR) ;
.. index::
   pair: ResetProc; (type)
.. code-block:: modula2
      ResetProc     = PROCEDURE (DeviceTablePtr) ;
.. index::
   pair: FlushProc; (type)
.. code-block:: modula2
      FlushProc     = PROCEDURE (DeviceTablePtr) ;
.. index::
   pair: FreeProc; (type)
.. code-block:: modula2
      FreeProc      = PROCEDURE (DeviceTablePtr) ;
         (* Carry out the operations involved in closing the corresponding
            channel, including flushing buffers, but do not unmake the
            channel.
         *)
    
    
    TYPE
.. index::
   pair: DeviceData; (type)
.. code-block:: modula2
      DeviceData = SYSTEM.ADDRESS;
    
.. index::
   pair: DeviceTable; (type)
.. code-block:: modula2
      DeviceTable =
        RECORD                         (* Initialized by MakeChan to: *)
          cd: DeviceData;              (* the value NIL *)
          did: DeviceId;               (* the value given in the call of MakeChan *)
          cid: IOChan.ChanId;          (* the identity of the channel *)
          result: IOConsts.ReadResults;(* the value notKnown *)
          errNum: IOChan.DeviceErrNum; (* undefined *)
          flags: ChanConsts.FlagSet;   (* ChanConsts.FlagSet{} *)
          doLook: LookProc;            (* raise exception notAvailable *)
          doSkip: SkipProc;            (* raise exception notAvailable *)
          doSkipLook: SkipLookProc;    (* raise exception notAvailable *)
          doLnWrite: WriteLnProc;      (* raise exception notAvailable *)
          doTextRead: TextReadProc;    (* raise exception notAvailable *)
          doTextWrite: TextWriteProc;  (* raise exception notAvailable *)
          doRawRead: RawReadProc;      (* raise exception notAvailable *)
          doRawWrite: RawWriteProc;    (* raise exception notAvailable *)
          doGetName: GetNameProc;      (* return the empty string *)
          doReset: ResetProc;          (* do nothing *)
          doFlush: FlushProc;          (* do nothing *)
          doFree: FreeProc;            (* do nothing *)
        END;
    
    
      (* The pointer to the device table for a channel is obtained using the
         following procedure: *)
    
    (*
       If the device module identified by did is not the module that made
       the channel identified by cid, the exception wrongDevice is raised.
    *)
    
.. index::
   DeviceTablePtrValue
.. code-block:: modula2
    PROCEDURE DeviceTablePtrValue (cid: IOChan.ChanId; did: DeviceId): DeviceTablePtr;
    
    
    (*
       Tests if the device module identified by did is the module
       that made the channel identified by cid.
    *)
    
.. index::
   IsDevice
.. code-block:: modula2
    PROCEDURE IsDevice (cid: IOChan.ChanId; did: DeviceId) : BOOLEAN;
    
    
    TYPE
.. index::
   pair: DevExceptionRange; (type)
.. code-block:: modula2
      DevExceptionRange = IOChan.ChanExceptions;
    
    (*
      ISO standard states defines
    
      DevExceptionRange = [IOChan.notAvailable ..  IOChan.textParseError];
    
      however this must be a bug as other modules need to raise
      IOChan.wrongDevice exceptions.
    *)
    
.. index::
   RAISEdevException
.. code-block:: modula2
    PROCEDURE RAISEdevException (cid: IOChan.ChanId; did: DeviceId;
                                 x: DevExceptionRange; s: ARRAY OF CHAR);
    
      (* If the device module identified by did is not the module that made the channel
         identified by cid, the exception wrongDevice is raised; otherwise the given exception
         is raised, and the string value in s is included in the exception message.
      *)
    
.. index::
   IsIOException
.. code-block:: modula2
    PROCEDURE IsIOException () : BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising af an exception from ChanExceptions;
         otherwise FALSE.
      *)
    
.. index::
   IOException
.. code-block:: modula2
    PROCEDURE IOException () : IOChan.ChanExceptions;
      (* If the current coroutine is in the exceptional execution state because of the
         raising af an exception from ChanExceptions, returns the corresponding
         enumeration value, and otherwise raises an exception.
      *)
    
    END IOLink.

@c @node gm2-libs-iso/IOResult, gm2-libs-iso/LongComplexMath, gm2-libs-iso/IOLink, M2 ISO Libraries
gm2-libs-iso/IOResult
---------------------

.. code-block:: modula2
    DEFINITION MODULE IOResult;

  (* Read results for specified channels *)
    
    IMPORT IOConsts, IOChan;
    
    TYPE
.. index::
   pair: ReadResults; (type)
.. code-block:: modula2
      ReadResults = IOConsts.ReadResults;
    
      (*
.. index::
   pair: ReadResults; (type)
.. code-block:: modula2
        ReadResults =  (* This type is used to classify the result of an input operation *)
        (
          notKnown,    (* no read result is set *)
          allRight,    (* data is as expected or as required *)
          outOfRange,  (* data cannot be represented *)
          wrongFormat, (* data not in expected format *)
          endOfLine,   (* end of line seen before expected data *)
          endOfInput   (* end of input seen before expected data *)
        );
      *)
    
.. index::
   ReadResult
.. code-block:: modula2
    PROCEDURE ReadResult (cid: IOChan.ChanId): ReadResults;
      (* Returns the result for the last read operation on the channel cid. *)
    
    END IOResult.
    

@c @node gm2-libs-iso/LongComplexMath, gm2-libs-iso/LongConv, gm2-libs-iso/IOResult, M2 ISO Libraries
gm2-libs-iso/LongComplexMath
----------------------------

.. code-block:: modula2
    DEFINITION MODULE LongComplexMath;

  (* Mathematical functions for the type LONGCOMPLEX *)
    
    CONST
.. index::
   pair: i; (const)
.. code-block:: modula2
      i =    CMPLX (0.0, 1.0);
.. index::
   pair: one; (const)
.. code-block:: modula2
      one =  CMPLX (1.0, 0.0);
.. index::
   pair: zero; (const)
.. code-block:: modula2
      zero = CMPLX (0.0, 0.0);
    
.. index::
   abs
.. code-block:: modula2
    PROCEDURE abs (z: LONGCOMPLEX): LONGREAL;
      (* Returns the length of z *)
    
.. index::
   arg
.. code-block:: modula2
    PROCEDURE arg (z: LONGCOMPLEX): LONGREAL;
      (* Returns the angle that z subtends to the positive real axis *)
    
.. index::
   conj
.. code-block:: modula2
    PROCEDURE conj (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the complex conjugate of z *)
    
.. index::
   power
.. code-block:: modula2
    PROCEDURE power (base: LONGCOMPLEX; exponent: LONGREAL): LONGCOMPLEX;
      (* Returns the value of the number base raised to the power exponent *)
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE sqrt (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the principal square root of z *)
    
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the complex exponential of z *)
    
.. index::
   ln
.. code-block:: modula2
    PROCEDURE ln (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the principal value of the natural logarithm of z *)
    
.. index::
   sin
.. code-block:: modula2
    PROCEDURE sin (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the sine of z *)
    
.. index::
   cos
.. code-block:: modula2
    PROCEDURE cos (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the cosine of z *)
    
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the tangent of z *)
    
.. index::
   arcsin
.. code-block:: modula2
    PROCEDURE arcsin (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the arcsine of z *)
    
.. index::
   arccos
.. code-block:: modula2
    PROCEDURE arccos (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the arccosine of z *)
    
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the arctangent of z *)
    
.. index::
   polarToComplex
.. code-block:: modula2
    PROCEDURE polarToComplex (abs, arg: LONGREAL): LONGCOMPLEX;
      (* Returns the complex number with the specified polar coordinates *)
    
.. index::
   scalarMult
.. code-block:: modula2
    PROCEDURE scalarMult (scalar: LONGREAL; z: LONGCOMPLEX): LONGCOMPLEX;
      (* Returns the scalar product of scalar with z *)
    
.. index::
   IsCMathException
.. code-block:: modula2
    PROCEDURE IsCMathException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising of an exception in a routine from this module; otherwise
         returns FALSE.
      *)
    
    END LongComplexMath.
    

@c @node gm2-libs-iso/LongConv, gm2-libs-iso/LongIO, gm2-libs-iso/LongComplexMath, M2 ISO Libraries
gm2-libs-iso/LongConv
---------------------

.. code-block:: modula2
    DEFINITION MODULE LongConv;

  (* Low-level LONGREAL/string conversions *)
    
    IMPORT
      ConvTypes;
    
    TYPE
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
      ConvResults = ConvTypes.ConvResults; (* strAllRight, strOutOfRange,
                                              strWrongFormat, strEmpty *)
    
.. index::
   ScanReal
.. code-block:: modula2
    PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                        VAR nextState: ConvTypes.ScanState);
      (* Represents the start state of a finite state scanner for real
         numbers - assigns class of inputCh to chClass and a procedure
         representing the next state to nextState.
      *)
    
.. index::
   FormatReal
.. code-block:: modula2
    PROCEDURE FormatReal (str: ARRAY OF CHAR): ConvResults;
      (* Returns the format of the string value for conversion to LONGREAL. *)
    
.. index::
   ValueReal
.. code-block:: modula2
    PROCEDURE ValueReal (str: ARRAY OF CHAR): LONGREAL;
      (* Returns the value corresponding to the real number string value
         str if str is well-formed; otherwise raises the LongConv exception.
      *)
    
.. index::
   LengthFloatReal
.. code-block:: modula2
    PROCEDURE LengthFloatReal (real: LONGREAL; sigFigs: CARDINAL): CARDINAL;
      (* Returns the number of characters in the floating-point string
         representation of real with sigFigs significant figures.
      *)
    
.. index::
   LengthEngReal
.. code-block:: modula2
    PROCEDURE LengthEngReal (real: LONGREAL; sigFigs: CARDINAL): CARDINAL;
      (* Returns the number of characters in the floating-point engineering
         string representation of real with sigFigs significant figures.
      *)
    
.. index::
   LengthFixedReal
.. code-block:: modula2
    PROCEDURE LengthFixedReal (real: LONGREAL; place: INTEGER): CARDINAL;
      (* Returns the number of characters in the fixed-point string
         representation of real rounded to the given place relative to the
         decimal point.
      *)
    
.. index::
   IsRConvException
.. code-block:: modula2
    PROCEDURE IsRConvException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception in a
         routine from this module; otherwise returns FALSE.
      *)
    
    END LongConv.
    

@c @node gm2-libs-iso/LongIO, gm2-libs-iso/LongMath, gm2-libs-iso/LongConv, M2 ISO Libraries
gm2-libs-iso/LongIO
-------------------

.. code-block:: modula2
    DEFINITION MODULE LongIO;

  (* Input and output of long real numbers in decimal text form
     over specified channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The text form of a signed fixed-point real number is
           ["+" | "-"], decimal digit, {decimal digit}, [".",
           {decimal digit}]
    
         The text form of a signed floating-point real number is
           signed fixed-point real number,
           "E", ["+" | "-"], decimal digit, {decimal digit}
      *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: LONGREAL);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of a signed fixed or floating
         point number.  The value of this number is assigned to real.
         The read result is set to the value allRight, outOfRange,
         wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteFloat
.. code-block:: modula2
    PROCEDURE WriteFloat (cid: IOChan.ChanId; real: LONGREAL;
                          sigFigs: CARDINAL; width: CARDINAL);
      (* Writes the value of real to cid in floating-point text form,
         with sigFigs significant figures, in a field of the given
         minimum width.
      *)
    
.. index::
   WriteEng
.. code-block:: modula2
    PROCEDURE WriteEng (cid: IOChan.ChanId; real: LONGREAL;
                        sigFigs: CARDINAL; width: CARDINAL);
      (* As for WriteFloat, except that the number is scaled with
         one to three digits in the whole number part, and with an
         exponent that is a multiple of three.
      *)
    
.. index::
   WriteFixed
.. code-block:: modula2
    PROCEDURE WriteFixed (cid: IOChan.ChanId; real: LONGREAL;
                          place: INTEGER; width: CARDINAL);
      (* Writes the value of real to cid in fixed-point text form,
         rounded to the given place relative to the decimal point,
         in a field of the given minimum width.
      *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (cid: IOChan.ChanId; real: LONGREAL;
                         width: CARDINAL);
      (* Writes the value of real to cid, as WriteFixed if the
         sign and magnitude can be shown in the given width, or
         otherwise as WriteFloat.  The number of places or
         significant digits depends on the given width.
      *)
    
    END LongIO.
    

@c @node gm2-libs-iso/LongMath, gm2-libs-iso/LongStr, gm2-libs-iso/LongIO, M2 ISO Libraries
gm2-libs-iso/LongMath
---------------------

.. code-block:: modula2
    DEFINITION MODULE LongMath;

  (* Mathematical functions for the type LONGREAL *)
    
    CONST
.. index::
   pair: pi; (const)
.. code-block:: modula2
      pi   = 3.1415926535897932384626433832795028841972;
.. index::
   pair: exp1; (const)
.. code-block:: modula2
      exp1 = 2.7182818284590452353602874713526624977572;
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: LONGREAL): LONGREAL;
      (* Returns the positive square root of x *)
    
.. index::
   exp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp (x: LONGREAL): LONGREAL;
      (* Returns the exponential of x *)
    
.. index::
   ln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ln (x: LONGREAL): LONGREAL;
      (* Returns the natural logarithm of x *)
    
      (* The angle in all trigonometric functions is measured in radians *)
    
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: LONGREAL): LONGREAL;
      (* Returns the sine of x *)
    
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: LONGREAL): LONGREAL;
      (* Returns the cosine of x *)
    
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (x: LONGREAL): LONGREAL;
      (* Returns the tangent of x *)
    
.. index::
   arcsin
.. code-block:: modula2
    PROCEDURE arcsin (x: LONGREAL): LONGREAL;
      (* Returns the arcsine of x *)
    
.. index::
   arccos
.. code-block:: modula2
    PROCEDURE arccos (x: LONGREAL): LONGREAL;
      (* Returns the arccosine of x *)
    
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (x: LONGREAL): LONGREAL;
      (* Returns the arctangent of x *)
    
.. index::
   power
.. code-block:: modula2
    PROCEDURE power (base, exponent: LONGREAL): LONGREAL;
      (* Returns the value of the number base raised to the power exponent *)
    
.. index::
   round
.. code-block:: modula2
    PROCEDURE round (x: LONGREAL): INTEGER;
      (* Returns the value of x rounded to the nearest integer *)
    
.. index::
   IsRMathException
.. code-block:: modula2
    PROCEDURE IsRMathException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception in a
         routine from this module; otherwise returns FALSE.
      *)
    
    END LongMath.
    

@c @node gm2-libs-iso/LongStr, gm2-libs-iso/LongWholeIO, gm2-libs-iso/LongMath, M2 ISO Libraries
gm2-libs-iso/LongStr
--------------------

.. code-block:: modula2
    DEFINITION MODULE LongStr;

  (* LONGREAL/string conversions *)
    
    IMPORT
       ConvTypes;
    
    TYPE
       (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
       ConvResults = ConvTypes.ConvResults;
    
    (* the string form of a signed fixed-point real number is
         ["+" | "-"], decimal digit, {decimal digit}, [".",
         {decimal digit}]
    *)
    
    (* the string form of a signed floating-point real number is
         signed fixed-point real number, "E", ["+" | "-"],
         decimal digit, {decimal digit}
    *)
    
.. index::
   StrToReal
.. code-block:: modula2
    PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: LONGREAL;
                         VAR res: ConvResults);
      (* Ignores any leading spaces in str. If the subsequent characters
         in str are in the format of a signed real number, assigns a
         corresponding value to real.  Assigns a value indicating the
         format of str to res.
      *)
    
.. index::
   RealToFloat
.. code-block:: modula2
    PROCEDURE RealToFloat (real: LONGREAL; sigFigs: CARDINAL;
                           VAR str: ARRAY OF CHAR);
      (* Converts the value of real to floating-point string form, with
         sigFigs significant figures, and copies the possibly truncated
         result to str.
      *)
    
.. index::
   RealToEng
.. code-block:: modula2
    PROCEDURE RealToEng (real: LONGREAL; sigFigs: CARDINAL;
                         VAR str: ARRAY OF CHAR);
      (* Converts the value of real to floating-point string form, with
         sigFigs significant figures, and copies the possibly truncated
         result to str. The number is scaled with one to three digits
         in the whole number part and with an exponent that is a
         multiple of three.
      *)
    
.. index::
   RealToFixed
.. code-block:: modula2
    PROCEDURE RealToFixed (real: LONGREAL; place: INTEGER;
                           VAR str: ARRAY OF CHAR);
      (* Converts the value of real to fixed-point string form, rounded
         to the given place relative to the decimal point, and copies
         the possibly truncated result to str.
      *)
    
.. index::
   RealToStr
.. code-block:: modula2
    PROCEDURE RealToStr (real: LONGREAL; VAR str: ARRAY OF CHAR);
      (* Converts the value of real as RealToFixed if the sign and
         magnitude can be shown within the capacity of str, or
         otherwise as RealToFloat, and copies the possibly truncated
         result to str. The number of places or significant digits
         depend on the capacity of str.
      *)
    
    END LongStr.
    

@c @node gm2-libs-iso/LongWholeIO, gm2-libs-iso/LowLong, gm2-libs-iso/LongStr, M2 ISO Libraries
gm2-libs-iso/LongWholeIO
------------------------

.. code-block:: modula2
    DEFINITION MODULE LongWholeIO;

  (* Input and output of whole numbers in decimal text form
     over specified channels.  The read result is of the
     type IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The text form of a signed whole number is
           ["+" | "-"], decimal digit, {decimal digit}
    
         The text form of an unsigned whole number is
           decimal digit, {decimal digit}
      *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (cid: IOChan.ChanId; VAR int: LONGINT);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of a signed whole number.  The
         value of this number is assigned to int.  The read result
         is set to the value allRight, outOfRange, wrongFormat,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (cid: IOChan.ChanId; int: LONGINT;
                        width: CARDINAL);
      (* Writes the value of int to cid in text form, in a field of
         the given minimum width. *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (cid: IOChan.ChanId; VAR card: LONGCARD);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of an unsigned whole number.  The
         value of this number is assigned to card. The read result
         is set to the value allRight, outOfRange, wrongFormat,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (cid: IOChan.ChanId; card: LONGCARD;
                         width: CARDINAL);
      (* Writes the value of card to cid in text form, in a field
         of the given minimum width. *)
    
    END LongWholeIO.

@c @node gm2-libs-iso/LowLong, gm2-libs-iso/LowReal, gm2-libs-iso/LongWholeIO, M2 ISO Libraries
gm2-libs-iso/LowLong
--------------------

.. code-block:: modula2
    DEFINITION MODULE LowLong;

  (* Access to underlying properties of the type LONGREAL *)
    
    CONST
.. index::
   pair: radix; (const)
.. code-block:: modula2
      radix      = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, radix> )) ;      (* ZType *)
.. index::
   pair: places; (const)
.. code-block:: modula2
      places     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, places> )) ;     (* ZType *)
.. index::
   pair: expoMin; (const)
.. code-block:: modula2
      expoMin    = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, expoMin> )) ;    (* ZType *)
.. index::
   pair: expoMax; (const)
.. code-block:: modula2
      expoMax    = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, expoMax> )) ;    (* ZType *)
.. index::
   pair: large; (const)
.. code-block:: modula2
      large      = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, large> )) ;      (* RType *)
.. index::
   pair: small; (const)
.. code-block:: modula2
      small      = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, small> )) ;      (* RType *)
.. index::
   pair: IEC559; (const)
.. code-block:: modula2
      IEC559     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, IEC559> )) ;     (* BOOLEAN *)
.. index::
   pair: LIA1; (const)
.. code-block:: modula2
      LIA1       = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, LIA1> )) ;       (* BOOLEAN *)
.. index::
   pair: ISO; (const)
.. code-block:: modula2
      ISO        = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, ISO> )) ;        (* BOOLEAN *)
.. index::
   pair: IEEE; (const)
.. code-block:: modula2
      IEEE       = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, IEEE> )) ;       (* BOOLEAN *)
.. index::
   pair: rounds; (const)
.. code-block:: modula2
      rounds     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, rounds> )) ;     (* BOOLEAN *)
.. index::
   pair: gUnderflow; (const)
.. code-block:: modula2
      gUnderflow = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, gUnderflow> )) ; (* BOOLEAN *)
.. index::
   pair: exception; (const)
.. code-block:: modula2
      exception  = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, exception> )) ;  (* BOOLEAN *)
.. index::
   pair: extend; (const)
.. code-block:: modula2
      extend     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, extend> )) ;     (* BOOLEAN *)
.. index::
   pair: nModes; (const)
.. code-block:: modula2
      nModes     = __ATTRIBUTE__ __BUILTIN__ (( <LONGREAL, nModes> )) ;     (* ZType *)
    
    TYPE
.. index::
   pair: Modes; (type)
.. code-block:: modula2
      Modes = PACKEDSET OF [0 .. nModes-1];
    
.. index::
   exponent
.. code-block:: modula2
    PROCEDURE exponent (x: LONGREAL): INTEGER;
      (* Returns the exponent value of x *)
    
.. index::
   fraction
.. code-block:: modula2
    PROCEDURE fraction (x: LONGREAL): LONGREAL;
      (* Returns the significand (or significant part) of x *)
    
.. index::
   sign
.. code-block:: modula2
    PROCEDURE sign (x: LONGREAL): LONGREAL;
      (* Returns the signum of x *)
    
.. index::
   succ
.. code-block:: modula2
    PROCEDURE succ (x: LONGREAL): LONGREAL;
      (* Returns the next value of the type LONGREAL greater than x *)
    
.. index::
   ulp
.. code-block:: modula2
    PROCEDURE ulp (x: LONGREAL): LONGREAL;
      (* Returns the value of a unit in the last place of x *)
    
.. index::
   pred
.. code-block:: modula2
    PROCEDURE pred (x: LONGREAL): LONGREAL;
      (* Returns the previous value of the type LONGREAL less than x *)
    
.. index::
   intpart
.. code-block:: modula2
    PROCEDURE intpart (x: LONGREAL): LONGREAL;
      (* Returns the integer part of x *)
    
.. index::
   fractpart
.. code-block:: modula2
    PROCEDURE fractpart (x: LONGREAL): LONGREAL;
      (* Returns the fractional part of x *)
    
.. index::
   scale
.. code-block:: modula2
    PROCEDURE scale (x: LONGREAL; n: INTEGER): LONGREAL;
      (* Returns the value of x * radix ** n *)
    
.. index::
   trunc
.. code-block:: modula2
    PROCEDURE trunc (x: LONGREAL; n: INTEGER): LONGREAL;
      (* Returns the value of the first n places of x *)
    
.. index::
   round
.. code-block:: modula2
    PROCEDURE round (x: LONGREAL; n: INTEGER): LONGREAL;
      (* Returns the value of x rounded to the first n places *)
    
.. index::
   synthesize
.. code-block:: modula2
    PROCEDURE synthesize (expart: INTEGER; frapart: LONGREAL): LONGREAL;
      (* Returns a value of the type LONGREAL constructed from the given expart and frapart *)
    
.. index::
   setMode
.. code-block:: modula2
    PROCEDURE setMode (m: Modes);
      (* Sets status flags appropriate to the underlying implementation of the type LONGREAL *)
    
.. index::
   currentMode
.. code-block:: modula2
    PROCEDURE currentMode (): Modes;
      (* Returns the current status flags in the form set by setMode *)
    
.. index::
   IsLowException
.. code-block:: modula2
    PROCEDURE IsLowException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising of an exception in a routine from this module; otherwise
         returns FALSE.
      *)
    
    END LowLong.
    

@c @node gm2-libs-iso/LowReal, gm2-libs-iso/LowShort, gm2-libs-iso/LowLong, M2 ISO Libraries
gm2-libs-iso/LowReal
--------------------

.. code-block:: modula2
    DEFINITION MODULE LowReal;

  (* Access to underlying properties of the type REAL *)
    
    CONST
.. index::
   pair: radix; (const)
.. code-block:: modula2
      radix      = __ATTRIBUTE__ __BUILTIN__ (( <REAL, radix> )) ;      (* ZType *)
.. index::
   pair: places; (const)
.. code-block:: modula2
      places     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, places> )) ;     (* ZType *)
.. index::
   pair: expoMin; (const)
.. code-block:: modula2
      expoMin    = __ATTRIBUTE__ __BUILTIN__ (( <REAL, expoMin> )) ;    (* ZType *)
.. index::
   pair: expoMax; (const)
.. code-block:: modula2
      expoMax    = __ATTRIBUTE__ __BUILTIN__ (( <REAL, expoMax> )) ;    (* ZType *)
.. index::
   pair: large; (const)
.. code-block:: modula2
      large      = __ATTRIBUTE__ __BUILTIN__ (( <REAL, large> )) ;      (* RType *)
.. index::
   pair: small; (const)
.. code-block:: modula2
      small      = __ATTRIBUTE__ __BUILTIN__ (( <REAL, small> )) ;      (* RType *)
.. index::
   pair: IEC559; (const)
.. code-block:: modula2
      IEC559     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, IEC559> )) ;     (* BOOLEAN *)
.. index::
   pair: LIA1; (const)
.. code-block:: modula2
      LIA1       = __ATTRIBUTE__ __BUILTIN__ (( <REAL, LIA1> )) ;       (* BOOLEAN *)
.. index::
   pair: ISO; (const)
.. code-block:: modula2
      ISO        = __ATTRIBUTE__ __BUILTIN__ (( <REAL, ISO> )) ;        (* BOOLEAN *)
.. index::
   pair: IEEE; (const)
.. code-block:: modula2
      IEEE       = __ATTRIBUTE__ __BUILTIN__ (( <REAL, IEEE> )) ;       (* BOOLEAN *)
.. index::
   pair: rounds; (const)
.. code-block:: modula2
      rounds     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, rounds> )) ;     (* BOOLEAN *)
.. index::
   pair: gUnderflow; (const)
.. code-block:: modula2
      gUnderflow = __ATTRIBUTE__ __BUILTIN__ (( <REAL, gUnderflow> )) ; (* BOOLEAN *)
.. index::
   pair: exception; (const)
.. code-block:: modula2
      exception  = __ATTRIBUTE__ __BUILTIN__ (( <REAL, exception> )) ;  (* BOOLEAN *)
.. index::
   pair: extend; (const)
.. code-block:: modula2
      extend     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, extend> )) ;     (* BOOLEAN *)
.. index::
   pair: nModes; (const)
.. code-block:: modula2
      nModes     = __ATTRIBUTE__ __BUILTIN__ (( <REAL, nModes> )) ;     (* ZType *)
    
    TYPE
.. index::
   pair: Modes; (type)
.. code-block:: modula2
      Modes = PACKEDSET OF [0..nModes-1];
    
.. index::
   exponent
.. code-block:: modula2
    PROCEDURE exponent (x: REAL): INTEGER;
      (* Returns the exponent value of x *)
    
.. index::
   fraction
.. code-block:: modula2
    PROCEDURE fraction (x: REAL): REAL;
      (* Returns the significand (or significant part) of x *)
    
.. index::
   sign
.. code-block:: modula2
    PROCEDURE sign (x: REAL): REAL;
      (* Returns the signum of x *)
    
.. index::
   succ
.. code-block:: modula2
    PROCEDURE succ (x: REAL): REAL;
      (* Returns the next value of the type REAL greater than x *)
    
.. index::
   ulp
.. code-block:: modula2
    PROCEDURE ulp (x: REAL): REAL;
      (* Returns the value of a unit in the last place of x *)
    
.. index::
   pred
.. code-block:: modula2
    PROCEDURE pred (x: REAL): REAL;
      (* Returns the previous value of the type REAL less than x *)
    
.. index::
   intpart
.. code-block:: modula2
    PROCEDURE intpart (x: REAL): REAL;
      (* Returns the integer part of x *)
    
.. index::
   fractpart
.. code-block:: modula2
    PROCEDURE fractpart (x: REAL): REAL;
      (* Returns the fractional part of x *)
    
.. index::
   scale
.. code-block:: modula2
    PROCEDURE scale (x: REAL; n: INTEGER): REAL;
      (* Returns the value of x * radix ** n *)
    
.. index::
   trunc
.. code-block:: modula2
    PROCEDURE trunc (x: REAL; n: INTEGER): REAL;
      (* Returns the value of the first n places of x *)
    
.. index::
   round
.. code-block:: modula2
    PROCEDURE round (x: REAL; n: INTEGER): REAL;
      (* Returns the value of x rounded to the first n places *)
    
.. index::
   synthesize
.. code-block:: modula2
    PROCEDURE synthesize (expart: INTEGER; frapart: REAL): REAL;
      (* Returns a value of the type REAL constructed from the given expart and frapart *)
    
.. index::
   setMode
.. code-block:: modula2
    PROCEDURE setMode (m: Modes);
      (* Sets status flags appropriate to the underlying implementation of the type REAL *)
    
.. index::
   currentMode
.. code-block:: modula2
    PROCEDURE currentMode (): Modes;
      (* Returns the current status flags in the form set by setMode *)
    
.. index::
   IsLowException
.. code-block:: modula2
    PROCEDURE IsLowException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising of an exception in a routine from this module; otherwise
         returns FALSE.
      *)
    
    END LowReal.
    

@c @node gm2-libs-iso/LowShort, gm2-libs-iso/M2EXCEPTION, gm2-libs-iso/LowReal, M2 ISO Libraries
gm2-libs-iso/LowShort
---------------------

.. code-block:: modula2
    DEFINITION MODULE LowShort;

  (* Access to underlying properties of the type SHORTREAL *)
    
    CONST
.. index::
   pair: radix; (const)
.. code-block:: modula2
      radix      = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, radix> )) ;      (* ZType *)
.. index::
   pair: places; (const)
.. code-block:: modula2
      places     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, places> )) ;     (* ZType *)
.. index::
   pair: expoMin; (const)
.. code-block:: modula2
      expoMin    = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, expoMin> )) ;    (* ZType *)
.. index::
   pair: expoMax; (const)
.. code-block:: modula2
      expoMax    = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, expoMax> )) ;    (* ZType *)
.. index::
   pair: large; (const)
.. code-block:: modula2
      large      = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, large> )) ;      (* RType *)
.. index::
   pair: small; (const)
.. code-block:: modula2
      small      = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, small> )) ;      (* RType *)
.. index::
   pair: IEC559; (const)
.. code-block:: modula2
      IEC559     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, IEC559> )) ;     (* BOOLEAN *)
.. index::
   pair: LIA1; (const)
.. code-block:: modula2
      LIA1       = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, LIA1> )) ;       (* BOOLEAN *)
.. index::
   pair: ISO; (const)
.. code-block:: modula2
      ISO        = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, ISO> )) ;        (* BOOLEAN *)
.. index::
   pair: IEEE; (const)
.. code-block:: modula2
      IEEE       = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, IEEE> )) ;       (* BOOLEAN *)
.. index::
   pair: rounds; (const)
.. code-block:: modula2
      rounds     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, rounds> )) ;     (* BOOLEAN *)
.. index::
   pair: gUnderflow; (const)
.. code-block:: modula2
      gUnderflow = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, gUnderflow> )) ; (* BOOLEAN *)
.. index::
   pair: exception; (const)
.. code-block:: modula2
      exception  = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, exception> )) ;  (* BOOLEAN *)
.. index::
   pair: extend; (const)
.. code-block:: modula2
      extend     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, extend> )) ;     (* BOOLEAN *)
.. index::
   pair: nModes; (const)
.. code-block:: modula2
      nModes     = __ATTRIBUTE__ __BUILTIN__ (( <SHORTREAL, nModes> )) ;     (* ZType *)
    
    TYPE
.. index::
   pair: Modes; (type)
.. code-block:: modula2
      Modes = PACKEDSET OF [0 .. nModes-1];
    
.. index::
   exponent
.. code-block:: modula2
    PROCEDURE exponent (x: SHORTREAL): INTEGER;
      (* Returns the exponent value of x *)
    
.. index::
   fraction
.. code-block:: modula2
    PROCEDURE fraction (x: SHORTREAL): SHORTREAL;
      (* Returns the significand (or significant part) of x *)
    
.. index::
   sign
.. code-block:: modula2
    PROCEDURE sign (x: SHORTREAL): SHORTREAL;
      (* Returns the signum of x *)
    
.. index::
   succ
.. code-block:: modula2
    PROCEDURE succ (x: SHORTREAL): SHORTREAL;
      (* Returns the next value of the type SHORTREAL greater than x *)
    
.. index::
   ulp
.. code-block:: modula2
    PROCEDURE ulp (x: SHORTREAL): SHORTREAL;
      (* Returns the value of a unit in the last place of x *)
    
.. index::
   pred
.. code-block:: modula2
    PROCEDURE pred (x: SHORTREAL): SHORTREAL;
      (* Returns the previous value of the type SHORTREAL less than x *)
    
.. index::
   intpart
.. code-block:: modula2
    PROCEDURE intpart (x: SHORTREAL): SHORTREAL;
      (* Returns the integer part of x *)
    
.. index::
   fractpart
.. code-block:: modula2
    PROCEDURE fractpart (x: SHORTREAL): SHORTREAL;
      (* Returns the fractional part of x *)
    
.. index::
   scale
.. code-block:: modula2
    PROCEDURE scale (x: SHORTREAL; n: INTEGER): SHORTREAL;
      (* Returns the value of x * radix ** n *)
    
.. index::
   trunc
.. code-block:: modula2
    PROCEDURE trunc (x: SHORTREAL; n: INTEGER): SHORTREAL;
      (* Returns the value of the first n places of x *)
    
.. index::
   round
.. code-block:: modula2
    PROCEDURE round (x: SHORTREAL; n: INTEGER): SHORTREAL;
      (* Returns the value of x rounded to the first n places *)
    
.. index::
   synthesize
.. code-block:: modula2
    PROCEDURE synthesize (expart: INTEGER; frapart: SHORTREAL): SHORTREAL;
      (* Returns a value of the type SHORTREAL constructed from the given expart and frapart *)
    
.. index::
   setMode
.. code-block:: modula2
    PROCEDURE setMode (m: Modes);
      (* Sets status flags appropriate to the underlying implementation of the type SHORTREAL *)
    
.. index::
   currentMode
.. code-block:: modula2
    PROCEDURE currentMode (): Modes;
      (* Returns the current status flags in the form set by setMode *)
    
.. index::
   IsLowException
.. code-block:: modula2
    PROCEDURE IsLowException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising of an exception in a routine from this module; otherwise
         returns FALSE.
      *)
    
    END LowShort.

@c @node gm2-libs-iso/M2EXCEPTION, gm2-libs-iso/M2RTS, gm2-libs-iso/LowShort, M2 ISO Libraries
gm2-libs-iso/M2EXCEPTION
------------------------

.. code-block:: modula2
    DEFINITION MODULE M2EXCEPTION;

(* Provides facilities for identifying language exceptions *)
    
    TYPE
.. index::
   pair: M2Exceptions; (type)
.. code-block:: modula2
      M2Exceptions =
        (indexException,     rangeException,         caseSelectException,  invalidLocation,
         functionException,  wholeValueException,    wholeDivException,    realValueException,
         realDivException,   complexValueException,  complexDivException,  protException,
         sysException,       coException,            exException
        );
    
.. index::
   M2Exception
.. code-block:: modula2
    PROCEDURE M2Exception (): M2Exceptions;
      (* If the current coroutine is in the exceptional execution state because of the raising
         of a language exception, returns the corresponding enumeration value, and otherwise
         raises an exception.
      *)
    
.. index::
   IsM2Exception
.. code-block:: modula2
    PROCEDURE IsM2Exception (): BOOLEAN;
      (* If the current coroutine is in the exceptional execution state because of the raising
         of a language exception, returns TRUE, and otherwise returns FALSE.
      *)
    
    END M2EXCEPTION.

@c @node gm2-libs-iso/M2RTS, gm2-libs-iso/MemStream, gm2-libs-iso/M2EXCEPTION, M2 ISO Libraries
gm2-libs-iso/M2RTS
------------------

.. code-block:: modula2
    DEFINITION MODULE M2RTS ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    
    TYPE
.. index::
   pair: ArgCVEnvP; (type)
.. code-block:: modula2
       ArgCVEnvP = PROCEDURE (INTEGER, ADDRESS, ADDRESS) ;
    
    
.. index::
   ConstructModules
.. code-block:: modula2
    PROCEDURE ConstructModules (applicationmodule: ADDRESS;
                                argc: INTEGER; argv, envp: ADDRESS) ;
    
.. index::
   DeconstructModules
.. code-block:: modula2
    PROCEDURE DeconstructModules (applicationmodule: ADDRESS;
                                  argc: INTEGER; argv, envp: ADDRESS) ;
    
    
    (*
       RegisterModule - adds module name to the list of outstanding
                        modules which need to have their dependencies
                        explored to determine initialization order.
    *)
    
.. index::
   RegisterModule
.. code-block:: modula2
    PROCEDURE RegisterModule (name: ADDRESS;
                              init, fini:  ArgCVEnvP;
                              dependencies: PROC) ;
    
    
    (*
       RequestDependant - used to specify that modulename is dependant upon
                          module dependantmodule.
    *)
    
.. index::
   RequestDependant
.. code-block:: modula2
    PROCEDURE RequestDependant (modulename, dependantmodule: ADDRESS) ;
    
    
    (*
       ExecuteTerminationProcedures - calls each installed termination
                                      procedure in reverse order.
    *)
    
.. index::
   ExecuteTerminationProcedures
.. code-block:: modula2
    PROCEDURE ExecuteTerminationProcedures ;
    
    
    (*
       InstallTerminationProcedure - installs a procedure, p, which will
                                     be called when the procedure
                                     ExecuteTerminationProcedures
                                     is invoked.  It returns TRUE is the
                                     procedure is installed.
    *)
    
.. index::
   InstallTerminationProcedure
.. code-block:: modula2
    PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;
    
    
    (*
       ExecuteInitialProcedures - executes the initial procedures installed
                                  by InstallInitialProcedure.
    *)
    
.. index::
   ExecuteInitialProcedures
.. code-block:: modula2
    PROCEDURE ExecuteInitialProcedures ;
    
    
    (*
       InstallInitialProcedure - installs a procedure to be executed just
                                 before the BEGIN code section of the main
                                 program module.
    *)
    
.. index::
   InstallInitialProcedure
.. code-block:: modula2
    PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;
    
    
    (*
       HALT - terminate the current program.  The procedure
              ExecuteTerminationProcedures
              is called before the program is stopped.  The parameter
              exitcode is optional.  If the parameter is not supplied
              HALT will call libc 'abort', otherwise it will exit with
              the code supplied.  Supplying a parameter to HALT has the
              same effect as calling ExitOnHalt with the same code and
              then calling HALT with no parameter.
    *)
    
.. index::
   HALT
.. code-block:: modula2
    PROCEDURE HALT ([exitcode: INTEGER = -1]) ;
    
    
    (*
       Halt - provides a more user friendly version of HALT, which takes
              four parameters to aid debugging.
    *)
    
.. index::
   Halt
.. code-block:: modula2
    PROCEDURE Halt (file: ARRAY OF CHAR; line: CARDINAL;
                    function: ARRAY OF CHAR; description: ARRAY OF CHAR) ;
    
    
    (*
       ExitOnHalt - if HALT is executed then call exit with the exit code, e.
    *)
    
.. index::
   ExitOnHalt
.. code-block:: modula2
    PROCEDURE ExitOnHalt (e: INTEGER) ;
    
    
    (*
       ErrorMessage - emits an error message to stderr and then calls exit (1).
    *)
    
.. index::
   ErrorMessage
.. code-block:: modula2
    PROCEDURE ErrorMessage (message: ARRAY OF CHAR;
                            file: ARRAY OF CHAR;
                            line: CARDINAL;
                            function: ARRAY OF CHAR) ;
    
    
    (*
       IsTerminating - Returns true if any coroutine has started program termination
                       and false otherwise.
    *)
    
.. index::
   IsTerminating
.. code-block:: modula2
    PROCEDURE IsTerminating () : BOOLEAN ;
    
    
    (*
       HasHalted - Returns true if a call to HALT has been made and false
                   otherwise.
    *)
    
.. index::
   HasHalted
.. code-block:: modula2
    PROCEDURE HasHalted () : BOOLEAN ;
    
    
    (*
       Length - returns the length of a string, a. This is called whenever
                the user calls LENGTH and the parameter cannot be calculated
                at compile time.
    *)
    
.. index::
   Length
.. code-block:: modula2
    PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
    
    
    (*
       The following are the runtime exception handler routines.
    *)
    
.. index::
   AssignmentException
.. code-block:: modula2
    PROCEDURE AssignmentException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ReturnException
.. code-block:: modula2
    PROCEDURE ReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   IncException
.. code-block:: modula2
    PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   DecException
.. code-block:: modula2
    PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   InclException
.. code-block:: modula2
    PROCEDURE InclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ExclException
.. code-block:: modula2
    PROCEDURE ExclException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ShiftException
.. code-block:: modula2
    PROCEDURE ShiftException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   RotateException
.. code-block:: modula2
    PROCEDURE RotateException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   StaticArraySubscriptException
.. code-block:: modula2
    PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   DynamicArraySubscriptException
.. code-block:: modula2
    PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ForLoopBeginException
.. code-block:: modula2
    PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ForLoopToException
.. code-block:: modula2
    PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ForLoopEndException
.. code-block:: modula2
    PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   PointerNilException
.. code-block:: modula2
    PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   NoReturnException
.. code-block:: modula2
    PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   CaseException
.. code-block:: modula2
    PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeNonPosDivException
.. code-block:: modula2
    PROCEDURE WholeNonPosDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeNonPosModException
.. code-block:: modula2
    PROCEDURE WholeNonPosModException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeZeroDivException
.. code-block:: modula2
    PROCEDURE WholeZeroDivException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeZeroRemException
.. code-block:: modula2
    PROCEDURE WholeZeroRemException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   WholeValueException
.. code-block:: modula2
    PROCEDURE WholeValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   RealValueException
.. code-block:: modula2
    PROCEDURE RealValueException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   ParameterException
.. code-block:: modula2
    PROCEDURE ParameterException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
.. index::
   NoException
.. code-block:: modula2
    PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope, message: ADDRESS) ;
    
    
    END M2RTS.

@c @node gm2-libs-iso/MemStream, gm2-libs-iso/Preemptive, gm2-libs-iso/M2RTS, M2 ISO Libraries
gm2-libs-iso/MemStream
----------------------

.. code-block:: modula2
    DEFINITION MODULE MemStream ;

(*
    Description: provides an ISO module which can write to a memory
                 buffer or read from a memory buffer.
*)
    
    FROM IOChan IMPORT ChanId ;
    FROM ChanConsts IMPORT FlagSet, OpenResults ;
    FROM SYSTEM IMPORT ADDRESS, LOC ;
    
    
    (*
       Attempts to obtain and open a channel connected to a contigeous
       buffer in memory.  The write flag is implied; without the raw
       flag, text is implied.  If successful, assigns to cid the identity of
       the opened channel, assigns the value opened to res.
       If a channel cannot be opened as required,
       the value of res indicates the reason, and cid identifies the
       invalid channel.
    
       The parameters, buffer, length and used maybe updated as
       data is written.  The buffer maybe reallocated
       and its address might alter, however the parameters will
       always reflect the current active buffer.  When this
       channel is closed the buffer is deallocated and
       buffer will be set to NIL, length and used will be set to
       zero.
    *)
    
.. index::
   OpenWrite
.. code-block:: modula2
    PROCEDURE OpenWrite (VAR cid: ChanId; flags: FlagSet;
                         VAR res: OpenResults;
                         VAR buffer: ADDRESS;
                         VAR length: CARDINAL;
                         VAR used: CARDINAL;
                         deallocOnClose: BOOLEAN) ;
    
    
    (*
       Attempts to obtain and open a channel connected to a contigeous
       buffer in memory.  The read and old flags are implied; without
       the raw flag, text is implied.  If successful, assigns to cid the
       identity of the opened channel, assigns the value opened to res, and
       selects input mode, with the read position corresponding to the start
       of the buffer.  If a channel cannot be opened as required, the value of
       res indicates the reason, and cid identifies the invalid channel.
    *)
    
.. index::
   OpenRead
.. code-block:: modula2
    PROCEDURE OpenRead (VAR cid: ChanId; flags: FlagSet;
                        VAR res: OpenResults;
                        buffer: ADDRESS; length: CARDINAL;
                        deallocOnClose: BOOLEAN) ;
    
    
    (*
       Close - if the channel identified by cid is not open to
               a memory stream, the exception wrongDevice is
               raised; otherwise closes the channel, and assigns
               the value identifying the invalid channel to cid.
    *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (VAR cid: ChanId) ;
    
    
    (*
       Rewrite - assigns the buffer index to zero.  Subsequent
                 writes will overwrite the previous buffer contents.
    *)
    
.. index::
   Rewrite
.. code-block:: modula2
    PROCEDURE Rewrite (cid: ChanId) ;
    
    
    (*
       Reread - assigns the buffer index to zero.  Subsequent
                reads will read the previous buffer contents.
    *)
    
.. index::
   Reread
.. code-block:: modula2
    PROCEDURE Reread (cid: ChanId) ;
    
    
    (*
       IsMem - tests if the channel identified by cid is open as
               a memory stream.
    *)
    
.. index::
   IsMem
.. code-block:: modula2
    PROCEDURE IsMem (cid: ChanId) : BOOLEAN ;
    
    
    END MemStream.

@c @node gm2-libs-iso/Preemptive, gm2-libs-iso/Processes, gm2-libs-iso/MemStream, M2 ISO Libraries
gm2-libs-iso/Preemptive
-----------------------

.. code-block:: modula2
    DEFINITION MODULE Preemptive ;

    
    (*
       initPreemptive - if microsecs > 0 then turn on preemptive scheduling.
                        if microsecs = 0 then preemptive scheduling is turned off.
    *)
    
.. index::
   initPreemptive
.. code-block:: modula2
    PROCEDURE initPreemptive (seconds, microsecs: CARDINAL) ;
    
    
    END Preemptive.

@c @node gm2-libs-iso/Processes, gm2-libs-iso/ProgramArgs, gm2-libs-iso/Preemptive, M2 ISO Libraries
gm2-libs-iso/Processes
----------------------

.. code-block:: modula2
    DEFINITION MODULE Processes;

  (* This module allows concurrent algorithms to be expressed using
     processes. A process is a unit of a program that has the
     potential to run in parallel with other processes.
  *)
    
    IMPORT SYSTEM;
    
    TYPE
      ProcessId;                      (* Used to identify processes *)
.. index::
   pair: Parameter; (type)
.. code-block:: modula2
      Parameter     = SYSTEM.ADDRESS; (* Used to pass data between processes *)
.. index::
   pair: Body; (type)
.. code-block:: modula2
      Body          = PROC;           (* Used as the type of a process body *)
.. index::
   pair: Urgency; (type)
.. code-block:: modula2
      Urgency       = INTEGER;        (* Used by the internal scheduler *)
.. index::
   pair: Sources; (type)
.. code-block:: modula2
      Sources       = CARDINAL;       (* Used to identify event sources *)
.. index::
   pair: ProcessesExceptions; (type)
.. code-block:: modula2
      ProcessesExceptions =           (* Exceptions raised by this module *)
        (passiveProgram, processError);
    
    (* The following procedures create processes and switch control between
       them. *)
    
.. index::
   Create
.. code-block:: modula2
    PROCEDURE Create (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                      procParams: Parameter; VAR procId: ProcessId);
      (* Creates a new process with procBody as its body, and with urgency
         and parameters given by procUrg and procParams.  At least as
         much workspace (in units of SYSTEM.LOC) as is specified by
         extraSpace is allocated to the process.
         An identity for the new process is returned in procId.
         The process is created in the passive state; it will not run
         until activated.
      *)
    
.. index::
   Start
.. code-block:: modula2
    PROCEDURE Start (procBody: Body; extraSpace: CARDINAL; procUrg: Urgency;
                     procParams: Parameter; VAR procId: ProcessId);
      (* Creates a new process, with parameters as for Create.
         The process is created in the ready state; it is eligible to
         run immediately.
      *)
    
.. index::
   StopMe
.. code-block:: modula2
    PROCEDURE StopMe ();
      (* Terminates the calling process.
         The process must not be associated with a source of events.
      *)
    
.. index::
   SuspendMe
.. code-block:: modula2
    PROCEDURE SuspendMe ();
      (* Causes the calling process to enter the passive state.  The
         procedure only returns when the calling process is again
         activated by another process.
      *)
    
.. index::
   Activate
.. code-block:: modula2
    PROCEDURE Activate (procId: ProcessId);
      (* Causes the process identified by procId to enter the ready
         state, and thus to become eligible to run again.
      *)
    
.. index::
   SuspendMeAndActivate
.. code-block:: modula2
    PROCEDURE SuspendMeAndActivate (procId: ProcessId);
      (* Executes an atomic sequence of SuspendMe() and
         Activate(procId). *)
    
.. index::
   Switch
.. code-block:: modula2
    PROCEDURE Switch (procId: ProcessId; VAR info: Parameter);
      (* Causes the calling process to enter the passive state; the
         process identified by procId becomes the currently executing
         process.  info is used to pass parameter information from the
         calling to the activated process.  On return, info will
         contain information from the process that chooses to switch
         back to this one (or will be NIL if Activate or
         SuspendMeAndActivate are used instead of Switch).
      *)
    
.. index::
   Wait
.. code-block:: modula2
    PROCEDURE Wait ();
      (* Causes the calling process to enter the waiting state.
         The procedure will return when the calling process is
         activated by another process, or when one of its associated
         eventSources has generated an event.
      *)
    
    (* The following procedures allow the association of processes
       with sources of external events.
    *)
    
.. index::
   Attach
.. code-block:: modula2
    PROCEDURE Attach (eventSource: Sources);
      (* Associates the specified eventSource with the calling
         process. *)
    
.. index::
   Detach
.. code-block:: modula2
    PROCEDURE Detach (eventSource: Sources);
      (* Dissociates the specified eventSource from the program. *)
    
.. index::
   IsAttached
.. code-block:: modula2
    PROCEDURE IsAttached (eventSource: Sources): BOOLEAN;
      (* Returns TRUE if and only if the specified eventSource is
         currently associated with one of the processes of the
         program.
      *)
    
.. index::
   Handler
.. code-block:: modula2
    PROCEDURE Handler (eventSource: Sources): ProcessId;
      (* Returns the identity of the process, if any, that is
         associated with the specified eventSource.
      *)
    
    (* The following procedures allow processes to obtain their
       identity, parameters, and urgency.
    *)
    
.. index::
   Me
.. code-block:: modula2
    PROCEDURE Me (): ProcessId;
      (* Returns the identity of the calling process (as assigned
         when the process was first created).
      *)
    
.. index::
   MyParam
.. code-block:: modula2
    PROCEDURE MyParam (): Parameter;
      (* Returns the value specified as procParams when the calling
         process was created. *)
    
.. index::
   UrgencyOf
.. code-block:: modula2
    PROCEDURE UrgencyOf (procId: ProcessId): Urgency;
      (* Returns the urgency established when the process identified
         by procId was first created.
      *)
    
    (* The following procedure provides facilities for exception
       handlers. *)
    
.. index::
   ProcessesException
.. code-block:: modula2
    PROCEDURE ProcessesException (): ProcessesExceptions;
      (* If the current coroutine is in the exceptional execution state
         because of the raising of a language exception, returns the
         corresponding enumeration value, and otherwise raises an
         exception.
      *)
    
.. index::
   IsProcessesException
.. code-block:: modula2
    PROCEDURE IsProcessesException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception in
         a routine from this module; otherwise returns FALSE.
      *)
    
    (*
       Reschedule - rotates the ready queue and transfers to the process
                    with the highest run priority.
    *)
    
.. index::
   Reschedule
.. code-block:: modula2
    PROCEDURE Reschedule ;
    
    
    (*
       displayProcesses -
    *)
    
.. index::
   displayProcesses
.. code-block:: modula2
    PROCEDURE displayProcesses (message: ARRAY OF CHAR) ;
    
    
    END Processes.

@c @node gm2-libs-iso/ProgramArgs, gm2-libs-iso/RTco, gm2-libs-iso/Processes, M2 ISO Libraries
gm2-libs-iso/ProgramArgs
------------------------

.. code-block:: modula2
    DEFINITION MODULE ProgramArgs;

  (* Access to program arguments *)
    
    IMPORT IOChan;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
      ChanId = IOChan.ChanId;
    
.. index::
   ArgChan
.. code-block:: modula2
    PROCEDURE ArgChan (): ChanId;
      (* Returns a value that identifies a channel for reading
         program arguments *)
    
.. index::
   IsArgPresent
.. code-block:: modula2
    PROCEDURE IsArgPresent (): BOOLEAN;
      (* Tests if there is a current argument to read from.  If not,
         read <= IOChan.CurrentFlags() will be FALSE, and attempting
         to read from the argument channel will raise the exception
         notAvailable.
      *)
    
.. index::
   NextArg
.. code-block:: modula2
    PROCEDURE NextArg ();
      (* If there is another argument, causes subsequent input from the
         argument device to come from the start of the next argument.
         Otherwise there is no argument to read from, and a call of
         IsArgPresent will return FALSE.
      *)
    
    END ProgramArgs.

@c @node gm2-libs-iso/RTco, gm2-libs-iso/RTdata, gm2-libs-iso/ProgramArgs, M2 ISO Libraries
gm2-libs-iso/RTco
-----------------

.. code-block:: modula2
    DEFINITION MODULE RTco ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    
    (* init initializes the module and allows the application to lazily invoke threads.  *)
    
.. index::
   init
.. code-block:: modula2
    PROCEDURE init () : INTEGER ;
    
.. index::
   initThread
.. code-block:: modula2
    PROCEDURE initThread (p: PROC; stackSize: CARDINAL; interruptLevel: CARDINAL) : INTEGER ;
    
.. index::
   initSemaphore
.. code-block:: modula2
    PROCEDURE initSemaphore (value: CARDINAL) : INTEGER ;
    
.. index::
   wait
.. code-block:: modula2
    PROCEDURE wait (semaphore: INTEGER) ;
    
.. index::
   signal
.. code-block:: modula2
    PROCEDURE signal (semaphore: INTEGER) ;
    
.. index::
   transfer
.. code-block:: modula2
    PROCEDURE transfer (VAR p1: INTEGER; p2: INTEGER) ;
    
.. index::
   waitThread
.. code-block:: modula2
    PROCEDURE waitThread (tid: INTEGER) ;
    
.. index::
   signalThread
.. code-block:: modula2
    PROCEDURE signalThread (tid: INTEGER) ;
    
.. index::
   currentThread
.. code-block:: modula2
    PROCEDURE currentThread () : INTEGER ;
    
    
    (* currentInterruptLevel returns the interrupt level of the current thread.  *)
    
.. index::
   currentInterruptLevel
.. code-block:: modula2
    PROCEDURE currentInterruptLevel () : CARDINAL ;
    
    
    (* turninterrupts returns the old interrupt level and assigns the interrupt level
       to newLevel.  *)
    
.. index::
   turnInterrupts
.. code-block:: modula2
    PROCEDURE turnInterrupts (newLevel: CARDINAL) : CARDINAL ;
    
    
    (*
       select access to the select system call which will be thread safe.
       This is typically called from the idle process to wait for an interrupt.
    *)
    
.. index::
   select
.. code-block:: modula2
    PROCEDURE select (p1: INTEGER;
                      p2: ADDRESS;
                      p3: ADDRESS;
                      p4: ADDRESS;
                      p5: ADDRESS) : INTEGER ;
    
    
    END RTco.

@c @node gm2-libs-iso/RTdata, gm2-libs-iso/RTentity, gm2-libs-iso/RTco, M2 ISO Libraries
gm2-libs-iso/RTdata
-------------------

.. code-block:: modula2
    DEFINITION MODULE RTdata ;

(*
    Description: provides a mechanism whereby devices can store
                 data attached to a device.
*)
    
    FROM SYSTEM IMPORT ADDRESS ;
    FROM IOLink IMPORT DeviceTablePtr ;
    
    TYPE
.. index::
   pair: ModuleId; (type)
.. code-block:: modula2
       ModuleId ;
.. index::
   pair: FreeProcedure; (type)
.. code-block:: modula2
       FreeProcedure = PROCEDURE (ADDRESS) ;
    
    
    (*
       MakeModuleId - creates a unique module Id.
    *)
    
.. index::
   MakeModuleId
.. code-block:: modula2
    PROCEDURE MakeModuleId (VAR m: ModuleId) ;
    
    
    (*
       InitData - adds, datum, to the device, d.  The datum
                  is associated with ModuleID, m.
    *)
    
.. index::
   InitData
.. code-block:: modula2
    PROCEDURE InitData (d: DeviceTablePtr; m: ModuleId;
                        datum: ADDRESS; f: FreeProcedure) ;
    
    
    (*
       GetData - returns the datum assocated with ModuleId, m.
    *)
    
.. index::
   GetData
.. code-block:: modula2
    PROCEDURE GetData (d: DeviceTablePtr; m: ModuleId) : ADDRESS ;
    
    
    (*
       KillData - destroys the datum associated with ModuleId, m,
                  in device, d.  It invokes the free procedure
                  given during InitData.
    *)
    
.. index::
   KillData
.. code-block:: modula2
    PROCEDURE KillData (d: DeviceTablePtr; m: ModuleId) ;
    
    
    END RTdata.

@c @node gm2-libs-iso/RTentity, gm2-libs-iso/RTfio, gm2-libs-iso/RTdata, M2 ISO Libraries
gm2-libs-iso/RTentity
---------------------

.. code-block:: modula2
    DEFINITION MODULE RTentity ;

(*
    Description: provides a set of routines for maintaining an
                 efficient mechanism to group opaque (or pointer)
                 data structures together.  Internally the
                 entities are grouped together using a binary
                 tree.  It does not use Storage - and instead
                 uses malloc, free from libc as Storage uses the
                 module to detect erroneous deallocations.
*)
    
    IMPORT SYSTEM ;
    
    TYPE
.. index::
   pair: Group; (type)
.. code-block:: modula2
       Group ;
    
    
.. index::
   InitGroup
.. code-block:: modula2
    PROCEDURE InitGroup () : Group ;
.. index::
   KillGroup
.. code-block:: modula2
    PROCEDURE KillGroup (g: Group) : Group ;
.. index::
   GetKey
.. code-block:: modula2
    PROCEDURE GetKey (g: Group; a: SYSTEM.ADDRESS) : CARDINAL ;
.. index::
   PutKey
.. code-block:: modula2
    PROCEDURE PutKey (g: Group; a: SYSTEM.ADDRESS; key: CARDINAL) ;
.. index::
   DelKey
.. code-block:: modula2
    PROCEDURE DelKey (g: Group; a: SYSTEM.ADDRESS) ;
.. index::
   IsIn
.. code-block:: modula2
    PROCEDURE IsIn (g: Group; a: SYSTEM.ADDRESS) : BOOLEAN ;
    
    
    END RTentity.

@c @node gm2-libs-iso/RTfio, gm2-libs-iso/RTgen, gm2-libs-iso/RTentity, M2 ISO Libraries
gm2-libs-iso/RTfio
------------------

.. code-block:: modula2
    DEFINITION MODULE RTfio ;

(*
    Description: provides default FIO based methods for the RTgenif
                 procedures.  These will be used by StreamFile,
                 SeqFile, StdChans, TermFile and RndFile.
*)
    
    FROM SYSTEM IMPORT ADDRESS ;
    FROM IOLink IMPORT DeviceTablePtr;
    FROM RTgenif IMPORT GenDevIF ;
    
    
    (*
       doreadchar - returns a CHAR from the file associated with, g.
    *)
    
.. index::
   doreadchar
.. code-block:: modula2
    PROCEDURE doreadchar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
    
    
    (*
       dounreadchar - pushes a CHAR back onto the file associated
                      with, g.
    *)
    
.. index::
   dounreadchar
.. code-block:: modula2
    PROCEDURE dounreadchar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
    
    
    (*
       dogeterrno - returns the errno relating to the generic device.
    *)
    
.. index::
   dogeterrno
.. code-block:: modula2
    PROCEDURE dogeterrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;
    
    
    (*
       dorbytes - reads upto, max, bytes setting, actual, and
                  returning FALSE if an error (not due to eof)
                  occurred.
    *)
    
.. index::
   dorbytes
.. code-block:: modula2
    PROCEDURE dorbytes (g: GenDevIF;
                        d: DeviceTablePtr;
                        to: ADDRESS;
                        max: CARDINAL;
                        VAR actual: CARDINAL) : BOOLEAN ;
    
    (*
       dowbytes - writes up to, nBytes.  It returns FALSE
                  if an error occurred and it sets actual
                  to the amount of data written.
    *)
    
.. index::
   dowbytes
.. code-block:: modula2
    PROCEDURE dowbytes (g: GenDevIF;
                        d: DeviceTablePtr;
                        from: ADDRESS;
                        nBytes: CARDINAL;
                        VAR actual: CARDINAL) : BOOLEAN ;
    
    
    (*
       dowriteln - attempt to write an end of line marker to the
                   file and returns TRUE if successful.
    *)
    
.. index::
   dowriteln
.. code-block:: modula2
    PROCEDURE dowriteln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       iseof - returns TRUE if end of file has been seen.
    *)
    
.. index::
   iseof
.. code-block:: modula2
    PROCEDURE iseof (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       iseoln - returns TRUE if end of line has been seen.
    *)
    
.. index::
   iseoln
.. code-block:: modula2
    PROCEDURE iseoln (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       iserror - returns TRUE if an error was seen on the device.
                 Note that reaching EOF is not classified as an
                 error.
    *)
    
.. index::
   iserror
.. code-block:: modula2
    PROCEDURE iserror (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    END RTfio.

@c @node gm2-libs-iso/RTgen, gm2-libs-iso/RTgenif, gm2-libs-iso/RTfio, M2 ISO Libraries
gm2-libs-iso/RTgen
------------------

.. code-block:: modula2
    DEFINITION MODULE RTgen ;

(*
    Description: provides a generic device interface between
                 ISO channels and the underlying PIM style
                 FIO procedure calls.
*)
    
    FROM RTgenif IMPORT GenDevIF ;
    FROM IOLink IMPORT DeviceId, DeviceTablePtr;
    FROM IOConsts IMPORT ReadResults ;
    FROM SYSTEM IMPORT ADDRESS ;
    
    
    TYPE
.. index::
   pair: ChanDev; (type)
.. code-block:: modula2
       ChanDev ;
.. index::
   pair: DeviceType; (type)
.. code-block:: modula2
       DeviceType = (seqfile, streamfile, programargs, stdchans, term, socket, rndfile) ;
    
    
    (*
       InitChanDev - initialize and return a ChanDev.
    *)
    
.. index::
   InitChanDev
.. code-block:: modula2
    PROCEDURE InitChanDev (t: DeviceType; d: DeviceId; g: GenDevIF) : ChanDev ;
    
    
    (*
       KillChanDev - deallocates, g.
    *)
    
.. index::
   KillChanDev
.. code-block:: modula2
    PROCEDURE KillChanDev (g: GenDevIF) : GenDevIF ;
    
    
    (*
       RaiseEOFinLook - returns TRUE if the Look procedure
                        should raise an exception if it
                        sees end of file.
    *)
    
.. index::
   RaiseEOFinLook
.. code-block:: modula2
    PROCEDURE RaiseEOFinLook (g: ChanDev) : BOOLEAN ;
    
    
    (*
       RaiseEOFinSkip - returns TRUE if the Skip procedure
                        should raise an exception if it
                        sees end of file.
    *)
    
.. index::
   RaiseEOFinSkip
.. code-block:: modula2
    PROCEDURE RaiseEOFinSkip (g: ChanDev) : BOOLEAN ;
    
    
.. index::
   doLook
.. code-block:: modula2
    PROCEDURE doLook (g: ChanDev;
                      d: DeviceTablePtr;
                      VAR ch: CHAR;
                      VAR r: ReadResults) ;
    
.. index::
   doSkip
.. code-block:: modula2
    PROCEDURE doSkip (g: ChanDev;
                      d: DeviceTablePtr) ;
    
.. index::
   doSkipLook
.. code-block:: modula2
    PROCEDURE doSkipLook (g: ChanDev;
                          d: DeviceTablePtr;
                          VAR ch: CHAR;
                          VAR r: ReadResults) ;
    
.. index::
   doWriteLn
.. code-block:: modula2
    PROCEDURE doWriteLn (g: ChanDev;
                         d: DeviceTablePtr) ;
    
.. index::
   doReadText
.. code-block:: modula2
    PROCEDURE doReadText (g: ChanDev;
                          d: DeviceTablePtr;
                          to: ADDRESS;
                          maxChars: CARDINAL;
                          VAR charsRead: CARDINAL) ;
    
.. index::
   doWriteText
.. code-block:: modula2
    PROCEDURE doWriteText (g: ChanDev;
                           d: DeviceTablePtr;
                           from: ADDRESS;
                           charsToWrite: CARDINAL) ;
    
.. index::
   doReadLocs
.. code-block:: modula2
    PROCEDURE doReadLocs (g: ChanDev;
                          d: DeviceTablePtr;
                          to: ADDRESS;
                          maxLocs: CARDINAL;
                          VAR locsRead: CARDINAL) ;
    
.. index::
   doWriteLocs
.. code-block:: modula2
    PROCEDURE doWriteLocs (g: ChanDev;
                           d: DeviceTablePtr;
                           from: ADDRESS;
                           locsToWrite: CARDINAL) ;
    
    (*
       checkErrno - checks a number of errno conditions and raises
                    appropriate ISO exceptions if they occur.
    *)
    
.. index::
   checkErrno
.. code-block:: modula2
    PROCEDURE checkErrno (g: ChanDev; d: DeviceTablePtr) ;
    
    
    END RTgen.

@c @node gm2-libs-iso/RTgenif, gm2-libs-iso/RTio, gm2-libs-iso/RTgen, M2 ISO Libraries
gm2-libs-iso/RTgenif
--------------------

.. code-block:: modula2
    DEFINITION MODULE RTgenif ;

(*
    Description: provides a generic interface mechanism used
                 by RTgen.  This is not an ISO module but rather
                 a runtime support module.
*)
    
    FROM SYSTEM IMPORT ADDRESS ;
    FROM IOLink IMPORT DeviceId, DeviceTablePtr ;
    
    TYPE
.. index::
   pair: GenDevIF; (type)
.. code-block:: modula2
       GenDevIF ;
.. index::
   pair: readchar; (type)
.. code-block:: modula2
       readchar   = PROCEDURE (GenDevIF, DeviceTablePtr) : CHAR ;
.. index::
   pair: unreadchar; (type)
.. code-block:: modula2
       unreadchar = PROCEDURE (GenDevIF, DeviceTablePtr, CHAR) : CHAR ;
.. index::
   pair: geterrno; (type)
.. code-block:: modula2
       geterrno   = PROCEDURE (GenDevIF, DeviceTablePtr) : INTEGER ;
.. index::
   pair: readbytes; (type)
.. code-block:: modula2
       readbytes  = PROCEDURE (GenDevIF, DeviceTablePtr, ADDRESS, CARDINAL, VAR CARDINAL) : BOOLEAN ;
.. index::
   pair: writebytes; (type)
.. code-block:: modula2
       writebytes = PROCEDURE (GenDevIF, DeviceTablePtr, ADDRESS, CARDINAL, VAR CARDINAL) : BOOLEAN ;
.. index::
   pair: writeln; (type)
.. code-block:: modula2
       writeln    = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
.. index::
   pair: iseof; (type)
.. code-block:: modula2
       iseof      = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
.. index::
   pair: iseoln; (type)
.. code-block:: modula2
       iseoln     = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
.. index::
   pair: iserror; (type)
.. code-block:: modula2
       iserror    = PROCEDURE (GenDevIF, DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       InitGenDevIF - initializes a generic device.
    *)
    
.. index::
   InitGenDevIF
.. code-block:: modula2
    PROCEDURE InitGenDevIF (d     : DeviceId;
                            rc    : readchar;
                            urc   : unreadchar;
                            geterr: geterrno;
                            rbytes: readbytes;
                            wbytes: writebytes;
                            wl    : writeln;
                            eof   : iseof;
                            eoln  : iseoln;
                            iserr : iserror) : GenDevIF ;
    
    
    (*
       getDID - returns the device id this generic interface.
    *)
    
.. index::
   getDID
.. code-block:: modula2
    PROCEDURE getDID (g: GenDevIF) : DeviceId ;
    
    
    (*
       doReadChar - returns the next character from the generic
                    device.
    *)
    
.. index::
   doReadChar
.. code-block:: modula2
    PROCEDURE doReadChar (g: GenDevIF; d: DeviceTablePtr) : CHAR ;
    
    
    (*
       doUnReadChar - pushes back a character to the generic device.
    *)
    
.. index::
   doUnReadChar
.. code-block:: modula2
    PROCEDURE doUnReadChar (g: GenDevIF; d: DeviceTablePtr; ch: CHAR) : CHAR ;
    
    
    (*
       doGetErrno - returns the errno relating to the generic device.
    *)
    
.. index::
   doGetErrno
.. code-block:: modula2
    PROCEDURE doGetErrno (g: GenDevIF; d: DeviceTablePtr) : INTEGER ;
    
    
    (*
       doRBytes - attempts to read, n, bytes from the generic device.
                  It set the actual amount read and returns a boolean
                  to determine whether an error occurred.
    *)
    
.. index::
   doRBytes
.. code-block:: modula2
    PROCEDURE doRBytes (g: GenDevIF; d: DeviceTablePtr;
                        to: ADDRESS; max: CARDINAL;
                        VAR actual: CARDINAL) : BOOLEAN ;
    
    
    (*
       doWBytes - attempts to write, n, bytes to the generic device.
                  It sets the actual amount written and returns a
                  boolean to determine whether an error occurred.
    *)
    
.. index::
   doWBytes
.. code-block:: modula2
    PROCEDURE doWBytes (g: GenDevIF; d: DeviceTablePtr;
                        from: ADDRESS; max: CARDINAL;
                        VAR actual: CARDINAL) : BOOLEAN ;
    
    
    (*
       doWrLn - writes an end of line marker and returns
                TRUE if successful.
    *)
    
.. index::
   doWrLn
.. code-block:: modula2
    PROCEDURE doWrLn (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       isEOF - returns true if the end of file was reached.
    *)
    
.. index::
   isEOF
.. code-block:: modula2
    PROCEDURE isEOF (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       isEOLN - returns true if the end of line was reached.
    *)
    
.. index::
   isEOLN
.. code-block:: modula2
    PROCEDURE isEOLN (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       isError - returns true if an error was seen in the device.
    *)
    
.. index::
   isError
.. code-block:: modula2
    PROCEDURE isError (g: GenDevIF; d: DeviceTablePtr) : BOOLEAN ;
    
    
    (*
       KillGenDevIF - deallocates a generic device.
    *)
    
.. index::
   KillGenDevIF
.. code-block:: modula2
    PROCEDURE KillGenDevIF (g: GenDevIF) : GenDevIF ;
    
    
    END RTgenif.

@c @node gm2-libs-iso/RTio, gm2-libs-iso/RandomNumber, gm2-libs-iso/RTgenif, M2 ISO Libraries
gm2-libs-iso/RTio
-----------------

.. code-block:: modula2
    DEFINITION MODULE RTio ;

(*
    Description: provides low level routines for creating and destroying
                 ChanIds.  This is necessary to allow multiple modules
                 to create, ChanId values, where ChanId is an opaque
                 type.
*)
    
    IMPORT FIO, IOLink ;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
       ChanId ;
    
    
    (*
       InitChanId - return a new ChanId.
    *)
    
.. index::
   InitChanId
.. code-block:: modula2
    PROCEDURE InitChanId () : ChanId ;
    
    
    (*
       KillChanId - deallocate a ChanId.
    *)
    
.. index::
   KillChanId
.. code-block:: modula2
    PROCEDURE KillChanId (c: ChanId) : ChanId ;
    
    
    (*
       NilChanId - return a NIL pointer.
    *)
    
.. index::
   NilChanId
.. code-block:: modula2
    PROCEDURE NilChanId () : ChanId ;
    
    
    (*
       GetDeviceId - returns the device id, from, c.
    *)
    
.. index::
   GetDeviceId
.. code-block:: modula2
    PROCEDURE GetDeviceId (c: ChanId) : IOLink.DeviceId ;
    
    
    (*
       SetDeviceId - sets the device id in, c.
    *)
    
.. index::
   SetDeviceId
.. code-block:: modula2
    PROCEDURE SetDeviceId (c: ChanId; d: IOLink.DeviceId) ;
    
    
    (*
       GetDevicePtr - returns the device table ptr, from, c.
    *)
    
.. index::
   GetDevicePtr
.. code-block:: modula2
    PROCEDURE GetDevicePtr (c: ChanId) : IOLink.DeviceTablePtr ;
    
    
    (*
       SetDevicePtr - sets the device table ptr in, c.
    *)
    
.. index::
   SetDevicePtr
.. code-block:: modula2
    PROCEDURE SetDevicePtr (c: ChanId; p: IOLink.DeviceTablePtr) ;
    
    
    (*
       GetFile - returns the file field from, c.
    *)
    
.. index::
   GetFile
.. code-block:: modula2
    PROCEDURE GetFile (c: ChanId) : FIO.File ;
    
    
    (*
       SetFile - sets the file field in, c.
    *)
    
.. index::
   SetFile
.. code-block:: modula2
    PROCEDURE SetFile (c: ChanId; f: FIO.File) ;
    
    
    END RTio.

@c @node gm2-libs-iso/RandomNumber, gm2-libs-iso/RawIO, gm2-libs-iso/RTio, M2 ISO Libraries
gm2-libs-iso/RandomNumber
-------------------------

.. code-block:: modula2
    DEFINITION MODULE RandomNumber ;

(*
    Description: provides primitives for obtaining random numbers on
                 pervasive data types.
*)
    
    FROM SYSTEM IMPORT BYTE ;
    EXPORT QUALIFIED Randomize, RandomInit, RandomBytes,
                     RandomCard, RandomShortCard, RandomLongCard,
                     RandomInt, RandomShortInt, RandomLongInt,
                     RandomReal, RandomLongReal, RandomShortReal ;
    
    
    (*
       Randomize - initialize the random number generator with a seed
                   based on the microseconds.
    *)
    
.. index::
   Randomize
.. code-block:: modula2
    PROCEDURE Randomize ;
    
    
    (*
       RandomInit - initialize the random number generator with value, seed.
    *)
    
.. index::
   RandomInit
.. code-block:: modula2
    PROCEDURE RandomInit (seed: CARDINAL) ;
    
    
    (*
       RandomBytes - fills in an array with random values.
    *)
    
.. index::
   RandomBytes
.. code-block:: modula2
    PROCEDURE RandomBytes (VAR a: ARRAY OF BYTE) ;
    
    
    (*
       RandomInt - return an INTEGER in the range [low .. high].
    *)
    
.. index::
   RandomInt
.. code-block:: modula2
    PROCEDURE RandomInt (low, high: INTEGER) : INTEGER ;
    
    
    (*
       RandomShortInt - return an SHORTINT in the range [low..high].
    *)
    
.. index::
   RandomShortInt
.. code-block:: modula2
    PROCEDURE RandomShortInt (low, high: SHORTINT) : SHORTINT ;
    
    
    (*
       RandomLongInt - return an LONGINT in the range [low..high].
    *)
    
.. index::
   RandomLongInt
.. code-block:: modula2
    PROCEDURE RandomLongInt (low, high: LONGINT) : LONGINT ;
    
    
    (*
       RandomShortCard - return a SHORTCARD in the range [low..high].
    *)
    
.. index::
   RandomShortCard
.. code-block:: modula2
    PROCEDURE RandomShortCard (low, high: CARDINAL) : CARDINAL ;
    
    
    (*
       RandomCard - return a CARDINAL in the range [low..high].
    *)
    
.. index::
   RandomCard
.. code-block:: modula2
    PROCEDURE RandomCard (low, high: CARDINAL) : CARDINAL ;
    
    
    (*
       RandomLongCard - return an LONGCARD in the range [low..high].
    *)
    
.. index::
   RandomLongCard
.. code-block:: modula2
    PROCEDURE RandomLongCard (low, high: LONGCARD) : LONGCARD ;
    
    
    (*
       RandomReal - return a REAL number in the range 0.0..1.0
    *)
    
.. index::
   RandomReal
.. code-block:: modula2
    PROCEDURE RandomReal () : REAL ;
    
    
    (*
       RandomShortReal - return a SHORTREAL number in the range 0.0..1.0
    *)
    
.. index::
   RandomShortReal
.. code-block:: modula2
    PROCEDURE RandomShortReal () : SHORTREAL ;
    
    
    (*
       RandomLongReal - return a LONGREAL number in the range 0.0..1.0
    *)
    
.. index::
   RandomLongReal
.. code-block:: modula2
    PROCEDURE RandomLongReal () : LONGREAL ;
    
    
    END RandomNumber.

@c @node gm2-libs-iso/RawIO, gm2-libs-iso/RealConv, gm2-libs-iso/RandomNumber, M2 ISO Libraries
gm2-libs-iso/RawIO
------------------

.. code-block:: modula2
    DEFINITION MODULE RawIO;

  (* Reading and writing data over specified channels using raw
     operations, that is, with no conversion or interpretation.
     The read result is of the type IOConsts.ReadResults.
  *)
    
    IMPORT IOChan, SYSTEM;
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (cid: IOChan.ChanId; VAR to: ARRAY OF SYSTEM.LOC);
      (* Reads storage units from cid, and assigns them to
         successive components of to. The read result is set
         to the value allRight, wrongFormat, or endOfInput.
      *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (cid: IOChan.ChanId; from: ARRAY OF SYSTEM.LOC);
      (* Writes storage units to cid from successive components
         of from. *)
    
    END RawIO.
    

@c @node gm2-libs-iso/RealConv, gm2-libs-iso/RealIO, gm2-libs-iso/RawIO, M2 ISO Libraries
gm2-libs-iso/RealConv
---------------------

.. code-block:: modula2
    DEFINITION MODULE RealConv;

  (* Low-level REAL/string conversions *)
    
    IMPORT
      ConvTypes;
    
    TYPE
      (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
      ConvResults = ConvTypes.ConvResults;
    
.. index::
   ScanReal
.. code-block:: modula2
    PROCEDURE ScanReal (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                        VAR nextState: ConvTypes.ScanState);
      (* Represents the start state of a finite state scanner for real
         numbers - assigns class of inputCh to chClass and a procedure
         representing the next state to nextState.
       *)
    
.. index::
   FormatReal
.. code-block:: modula2
    PROCEDURE FormatReal (str: ARRAY OF CHAR): ConvResults;
      (* Returns the format of the string value for conversion to REAL. *)
    
.. index::
   ValueReal
.. code-block:: modula2
    PROCEDURE ValueReal (str: ARRAY OF CHAR): REAL;
      (* Returns the value corresponding to the real number string value
         str if str is well-formed; otherwise raises the RealConv
         exception.
      *)
    
.. index::
   LengthFloatReal
.. code-block:: modula2
    PROCEDURE LengthFloatReal (real: REAL; sigFigs: CARDINAL): CARDINAL;
      (* Returns the number of characters in the floating-point string
         representation of real with sigFigs significant figures.
      *)
    
.. index::
   LengthEngReal
.. code-block:: modula2
    PROCEDURE LengthEngReal (real: REAL; sigFigs: CARDINAL): CARDINAL;
      (* Returns the number of characters in the floating-point engineering
         string representation of real with sigFigs significant figures.
      *)
    
.. index::
   LengthFixedReal
.. code-block:: modula2
    PROCEDURE LengthFixedReal (real: REAL; place: INTEGER): CARDINAL;
      (* Returns the number of characters in the fixed-point string
         representation of real rounded to the given place relative to the
         decimal point.
      *)
    
.. index::
   IsRConvException
.. code-block:: modula2
    PROCEDURE IsRConvException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception in a
         routine from this module; otherwise returns FALSE.
      *)
    
    END RealConv.

@c @node gm2-libs-iso/RealIO, gm2-libs-iso/RealMath, gm2-libs-iso/RealConv, M2 ISO Libraries
gm2-libs-iso/RealIO
-------------------

.. code-block:: modula2
    DEFINITION MODULE RealIO;

  (* Input and output of real numbers in decimal text form
     over specified channels.  The read result is of the
     type IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The text form of a signed fixed-point real number is
           ["+" | "-"], decimal digit, {decimal digit},
           [".", {decimal digit}]
    
         The text form of a signed floating-point real number is
           signed fixed-point real number,
           "E", ["+" | "-"], decimal digit, {decimal digit}
      *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: REAL);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of a signed fixed or floating
         point number.  The value of this number is assigned to real.
         The read result is set to the value allRight, outOfRange,
         wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteFloat
.. code-block:: modula2
    PROCEDURE WriteFloat (cid: IOChan.ChanId; real: REAL;
                          sigFigs: CARDINAL; width: CARDINAL);
      (* Writes the value of real to cid in floating-point text form,
         with sigFigs significant figures, in a field of the given
         minimum width.
      *)
    
.. index::
   WriteEng
.. code-block:: modula2
    PROCEDURE WriteEng (cid: IOChan.ChanId; real: REAL;
                        sigFigs: CARDINAL; width: CARDINAL);
      (* As for WriteFloat, except that the number is scaled with
         one to three digits in the whole number part, and with an
         exponent that is a multiple of three.
      *)
    
.. index::
   WriteFixed
.. code-block:: modula2
    PROCEDURE WriteFixed (cid: IOChan.ChanId; real: REAL;
                          place: INTEGER; width: CARDINAL);
      (* Writes the value of real to cid in fixed-point text form,
         rounded to the given place relative to the decimal point,
         in a field of the given minimum width.
      *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (cid: IOChan.ChanId;
                         real: REAL; width: CARDINAL);
      (* Writes the value of real to cid, as WriteFixed if the sign
         and magnitude can be shown in the given width, or otherwise
         as WriteFloat.  The number of places or significant digits
         depends on the given width.
      *)
    
    END RealIO.

@c @node gm2-libs-iso/RealMath, gm2-libs-iso/RealStr, gm2-libs-iso/RealIO, M2 ISO Libraries
gm2-libs-iso/RealMath
---------------------

.. code-block:: modula2
    DEFINITION MODULE RealMath;

  (* Mathematical functions for the type REAL *)
    
    CONST
.. index::
   pair: pi; (const)
.. code-block:: modula2
      pi   = 3.1415926535897932384626433832795028841972;
.. index::
   pair: exp1; (const)
.. code-block:: modula2
      exp1 = 2.7182818284590452353602874713526624977572;
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: REAL): REAL;
      (* Returns the positive square root of x *)
    
.. index::
   exp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp (x: REAL): REAL;
      (* Returns the exponential of x *)
    
.. index::
   ln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ln (x: REAL): REAL;
      (* Returns the natural logarithm of x *)
    
      (* The angle in all trigonometric functions is measured in radians *)
    
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: REAL): REAL;
      (* Returns the sine of x *)
    
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: REAL): REAL;
      (* Returns the cosine of x *)
    
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (x: REAL): REAL;
      (* Returns the tangent of x *)
    
.. index::
   arcsin
.. code-block:: modula2
    PROCEDURE arcsin (x: REAL): REAL;
      (* Returns the arcsine of x *)
    
.. index::
   arccos
.. code-block:: modula2
    PROCEDURE arccos (x: REAL): REAL;
      (* Returns the arccosine of x *)
    
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (x: REAL): REAL;
      (* Returns the arctangent of x *)
    
.. index::
   power
.. code-block:: modula2
    PROCEDURE power (base, exponent: REAL) : REAL;
      (* Returns the value of the number base raised to the power exponent *)
    
.. index::
   round
.. code-block:: modula2
    PROCEDURE round (x: REAL) : INTEGER;
      (* Returns the value of x rounded to the nearest integer *)
    
.. index::
   IsRMathException
.. code-block:: modula2
    PROCEDURE IsRMathException () : BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising of an exception in a routine from this module; otherwise
         returns FALSE.
      *)
    
    END RealMath.
    

@c @node gm2-libs-iso/RealStr, gm2-libs-iso/RndFile, gm2-libs-iso/RealMath, M2 ISO Libraries
gm2-libs-iso/RealStr
--------------------

.. code-block:: modula2
    DEFINITION MODULE RealStr;

  (* REAL/string conversions *)
    
    IMPORT
      ConvTypes;
    
    TYPE
      (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
      ConvResults = ConvTypes.ConvResults;
    
    (* the string form of a signed fixed-point real number is
         ["+" | "-"], decimal digit, {decimal digit}, [".",
         {decimal digit}]
    *)
    
    (* the string form of a signed floating-point real number is
         signed fixed-point real number, "E", ["+" | "-"],
         decimal digit, {decimal digit}
    *)
    
.. index::
   StrToReal
.. code-block:: modula2
    PROCEDURE StrToReal (str: ARRAY OF CHAR; VAR real: REAL;
                         VAR res: ConvResults);
      (* Ignores any leading spaces in str. If the subsequent characters
         in str are in the format of a signed real number, assigns a
         corresponding value to real.  Assigns a value indicating the
         format of str to res.
      *)
    
.. index::
   RealToFloat
.. code-block:: modula2
    PROCEDURE RealToFloat (real: REAL; sigFigs: CARDINAL;
                           VAR str: ARRAY OF CHAR);
      (* Converts the value of real to floating-point string form, with
         sigFigs significant figures, and copies the possibly truncated
         result to str.
      *)
    
.. index::
   RealToEng
.. code-block:: modula2
    PROCEDURE RealToEng (real: REAL; sigFigs: CARDINAL;
                         VAR str: ARRAY OF CHAR);
      (* Converts the value of real to floating-point string form, with
         sigFigs significant figures, and copies the possibly truncated
         result to str.  The number is scaled with one to three digits
         in the whole number part and with an exponent that is a multiple
         of three.
      *)
    
.. index::
   RealToFixed
.. code-block:: modula2
    PROCEDURE RealToFixed (real: REAL; place: INTEGER;
                           VAR str: ARRAY OF CHAR);
      (* Converts the value of real to fixed-point string form, rounded
         to the given place relative to the decimal point, and copies
         the possibly truncated result to str.
      *)
    
.. index::
   RealToStr
.. code-block:: modula2
    PROCEDURE RealToStr (real: REAL; VAR str: ARRAY OF CHAR);
      (* Converts the value of real as RealToFixed if the sign and
         magnitude can be shown within the capacity of str, or
         otherwise as RealToFloat, and copies the possibly truncated
         result to str. The number of places or significant digits are
         implementation-defined.
      *)
    
    END RealStr.
    

@c @node gm2-libs-iso/RndFile, gm2-libs-iso/SIOResult, gm2-libs-iso/RealStr, M2 ISO Libraries
gm2-libs-iso/RndFile
--------------------

.. code-block:: modula2
    DEFINITION MODULE RndFile;

  (* Random access files *)
    
    IMPORT IOChan, ChanConsts, SYSTEM;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
       ChanId = IOChan.ChanId;
.. index::
   pair: FlagSet; (type)
.. code-block:: modula2
       FlagSet = ChanConsts.FlagSet;
.. index::
   pair: OpenResults; (type)
.. code-block:: modula2
       OpenResults = ChanConsts.OpenResults;
    
       (* Accepted singleton values of FlagSet *)
    
    CONST
       (* input operations are requested/available *)
.. index::
   pair: read; (const)
.. code-block:: modula2
       read = FlagSet{ChanConsts.readFlag};
       (* output operations are requested/available *)
.. index::
   pair: write; (const)
.. code-block:: modula2
       write = FlagSet{ChanConsts.writeFlag};
       (* a file may/must/did exist before the channel is opened *)
.. index::
   pair: old; (const)
.. code-block:: modula2
       old = FlagSet{ChanConsts.oldFlag};
       (* text operations are requested/available *)
.. index::
   pair: text; (const)
.. code-block:: modula2
       text = FlagSet{ChanConsts.textFlag};
       (* raw operations are requested/available *)
.. index::
   pair: raw; (const)
.. code-block:: modula2
       raw = FlagSet{ChanConsts.rawFlag};
    
.. index::
   OpenOld
.. code-block:: modula2
    PROCEDURE OpenOld (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                       VAR res: OpenResults);
      (* Attempts to obtain and open a channel connected to a stored random
         access file of the given name.
         The old flag is implied; without the write flag, read is implied;
         without the text flag, raw is implied.
         If successful, assigns to cid the identity of the opened channel,
         assigns the value opened to res, and sets the read/write position
         to the start of the file.
         If a channel cannot be opened as required, the value of res indicates
         the reason, and cid identifies the invalid channel.
      *)
    
.. index::
   OpenClean
.. code-block:: modula2
    PROCEDURE OpenClean (VAR cid: ChanId; name: ARRAY OF CHAR; flags: FlagSet;
                         VAR res: OpenResults);
      (* Attempts to obtain and open a channel connected to a stored random
         access file of the given name.
         The write flag is implied; without the text flag, raw is implied.
         If successful, assigns to cid the identity of the opened channel,
         assigns the value opened to res, and truncates the file to zero length.
         If a channel cannot be opened as required, the value of res indicates
         the reason, and cid identifies the invalid channel.
      *)
    
.. index::
   IsRndFile
.. code-block:: modula2
    PROCEDURE IsRndFile (cid: ChanId): BOOLEAN;
      (* Tests if the channel identified by cid is open to a random access file. *)
    
.. index::
   IsRndFileException
.. code-block:: modula2
    PROCEDURE IsRndFileException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution
         state because of the raising of a RndFile exception; otherwise returns
         FALSE.
      *)
    
    CONST
.. index::
   pair: FilePosSize; (const)
.. code-block:: modula2
       FilePosSize = SIZE(LONGINT) ;
       (* <implementation-defined whole number greater than zero>; *)
    
    TYPE
.. index::
   pair: FilePos; (type)
.. code-block:: modula2
       FilePos = LONGINT ;  (* ARRAY [1 .. FilePosSize] OF SYSTEM.LOC; *)
    
.. index::
   StartPos
.. code-block:: modula2
    PROCEDURE StartPos (cid: ChanId): FilePos;
      (* If the channel identified by cid is not open to a random access file,
         the exception wrongDevice is raised; otherwise returns the position of
         the start of the file.
      *)
    
.. index::
   CurrentPos
.. code-block:: modula2
    PROCEDURE CurrentPos (cid: ChanId): FilePos;
      (* If the channel identified by cid is not open to a random access file,
         the exception wrongDevice is raised; otherwise returns the position
         of the current read/write position.
      *)
    
.. index::
   EndPos
.. code-block:: modula2
    PROCEDURE EndPos (cid: ChanId): FilePos;
      (* If the channel identified by cid is not open to a random access file,
         the exception wrongDevice is raised; otherwise returns the first
         position after which there have been no writes.
      *)
    
.. index::
   NewPos
.. code-block:: modula2
    PROCEDURE NewPos (cid: ChanId; chunks: INTEGER; chunkSize: CARDINAL;
                      from: FilePos): FilePos;
      (* If the channel identified by cid is not open to a random access file,
         the exception wrongDevice is raised; otherwise returns the position
         (chunks * chunkSize) relative to the position given by from, or
         raises the exception posRange if the required position cannot be
         represented as a value of type FilePos.
      *)
    
.. index::
   SetPos
.. code-block:: modula2
    PROCEDURE SetPos (cid: ChanId; pos: FilePos);
      (* If the channel identified by cid is not open to a random access file,
         the exception wrongDevice is raised; otherwise sets the read/write
         position to the value given by pos.
      *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (VAR cid: ChanId);
      (* If the channel identified by cid is not open to a random access file,
         the exception wrongDevice is raised; otherwise closes the channel,
         and assigns the value identifying the invalid channel to cid.
      *)
    
    END RndFile.

@c @node gm2-libs-iso/SIOResult, gm2-libs-iso/SLongIO, gm2-libs-iso/RndFile, M2 ISO Libraries
gm2-libs-iso/SIOResult
----------------------

.. code-block:: modula2
    DEFINITION MODULE SIOResult;

  (* Read results for the default input channel *)
    
    IMPORT IOConsts;
    
    TYPE
.. index::
   pair: ReadResults; (type)
.. code-block:: modula2
      ReadResults = IOConsts.ReadResults;
    
      (*
.. index::
   pair: ReadResults; (type)
.. code-block:: modula2
        ReadResults =   (* This type is used to classify the result of an input operation *)
        (
          notKnown,     (* no read result is set *)
          allRight,     (* data is as expected or as required *)
          outOfRange,   (* data cannot be represented *)
          wrongFormat,  (* data not in expected format *)
          endOfLine,    (* end of line seen before expected data *)
          endOfInput    (* end of input seen before expected data *)
        );
      *)
    
.. index::
   ReadResult
.. code-block:: modula2
    PROCEDURE ReadResult (): ReadResults;
      (* Returns the result for the last read operation on the default input channel. *)
    
    END SIOResult.
    

@c @node gm2-libs-iso/SLongIO, gm2-libs-iso/SLongWholeIO, gm2-libs-iso/SIOResult, M2 ISO Libraries
gm2-libs-iso/SLongIO
--------------------

.. code-block:: modula2
    DEFINITION MODULE SLongIO;

  (* Input and output of long real numbers in decimal text form
     using default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
      (* The text form of a signed fixed-point real number is
           ["+" | "-"], decimal digit, {decimal digit},
           [".", {decimal digit}]
    
         The text form of a signed floating-point real number is
           signed fixed-point real number,
           "E", ["+" | "-"], decimal digit, {decimal digit}
      *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (VAR real: LONGREAL);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of a signed
         fixed or floating point number. The value of this number
         is assigned to real.  The read result is set to the value
         allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteFloat
.. code-block:: modula2
    PROCEDURE WriteFloat (real: LONGREAL; sigFigs: CARDINAL;
                          width: CARDINAL);
      (* Writes the value of real to the default output channel in
         floating-point text form, with sigFigs significant figures,
         in a field of the given minimum width.
      *)
    
.. index::
   WriteEng
.. code-block:: modula2
    PROCEDURE WriteEng (real: LONGREAL; sigFigs: CARDINAL;
                        width: CARDINAL);
      (* As for WriteFloat, except that the number is scaled with
         one to three digits in the whole number part, and with an
         exponent that is a multiple of three.
      *)
    
.. index::
   WriteFixed
.. code-block:: modula2
    PROCEDURE WriteFixed (real: LONGREAL; place: INTEGER;
                          width: CARDINAL);
      (* Writes the value of real to the default output channel in
         fixed-point text form, rounded to the given place relative
         to the decimal point, in a field of the given minimum width.
      *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (real: LONGREAL; width: CARDINAL);
      (* Writes the value of real to the default output channel, as
         WriteFixed if the sign and magnitude can be shown in the
         given width, or otherwise as WriteFloat. The number of
         places or significant digits depends on the given width.
      *)
    
    END SLongIO.
    

@c @node gm2-libs-iso/SLongWholeIO, gm2-libs-iso/SRawIO, gm2-libs-iso/SLongIO, M2 ISO Libraries
gm2-libs-iso/SLongWholeIO
-------------------------

.. code-block:: modula2
    DEFINITION MODULE SLongWholeIO;

  (* Input and output of whole numbers in decimal text form over
     default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
      (* The text form of a signed whole number is
           ["+" | "-"], decimal digit, {decimal digit}
    
         The text form of an unsigned whole number is
           decimal digit, {decimal digit}
      *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (VAR int: LONGINT);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of a signed
         whole number.  The value of this number is assigned
         to int.  The read result is set to the value allRight,
         outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (int: LONGINT; width: CARDINAL);
      (* Writes the value of int to the default output channel in
         text form, in a field of the given minimum width.
      *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (VAR card: LONGCARD);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of an
         unsigned whole number.  The value of this number is
         assigned to card.  The read result is set to the value
         allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (card: LONGCARD; width: CARDINAL);
      (* Writes the value of card to the default output channel in
         text form, in a field of the given minimum width.
      *)
    
    END SLongWholeIO.

@c @node gm2-libs-iso/SRawIO, gm2-libs-iso/SRealIO, gm2-libs-iso/SLongWholeIO, M2 ISO Libraries
gm2-libs-iso/SRawIO
-------------------

.. code-block:: modula2
    DEFINITION MODULE SRawIO;

  (* Reading and writing data over default channels using raw operations, that is, with no
     conversion or interpretation. The read result is of the type IOConsts.ReadResults.
  *)
    
    IMPORT SYSTEM;
    
.. index::
   Read
.. code-block:: modula2
    PROCEDURE Read (VAR to: ARRAY OF SYSTEM.LOC);
      (* Reads storage units from the default input channel, and assigns them to successive
         components of to.  The read result is set to the value allRight, wrongFormat, or
         endOfInput.
      *)
    
.. index::
   Write
.. code-block:: modula2
    PROCEDURE Write (from: ARRAY OF SYSTEM.LOC);
      (* Writes storage units to the default output channel from successive components of from.
      *)
    
    END SRawIO.
    

@c @node gm2-libs-iso/SRealIO, gm2-libs-iso/SShortIO, gm2-libs-iso/SRawIO, M2 ISO Libraries
gm2-libs-iso/SRealIO
--------------------

.. code-block:: modula2
    DEFINITION MODULE SRealIO;

  (* Input and output of real numbers in decimal text form over
     default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
      (* The text form of a signed fixed-point real number is
           ["+" | "-"], decimal digit, {decimal digit},
           [".", {decimal digit}]
    
         The text form of a signed floating-point real number is
           signed fixed-point real number,
           "E", ["+" | "-"], decimal digit, {decimal digit}
      *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (VAR real: REAL);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of a signed
         fixed or floating point number. The value of this number
         is assigned to real.  The read result is set to the value
         allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteFloat
.. code-block:: modula2
    PROCEDURE WriteFloat (real: REAL; sigFigs: CARDINAL; width: CARDINAL);
      (* Writes the value of real to the default output channel in
         floating-point text form, with sigFigs significant figures,
         in a field of the given minimum width.
      *)
    
.. index::
   WriteEng
.. code-block:: modula2
    PROCEDURE WriteEng (real: REAL; sigFigs: CARDINAL; width: CARDINAL);
      (* As for WriteFloat, except that the number is scaled with one to
         three digits in the whole number part, and with an exponent that
         is a multiple of three.
      *)
    
.. index::
   WriteFixed
.. code-block:: modula2
    PROCEDURE WriteFixed (real: REAL; place: INTEGER; width: CARDINAL);
      (* Writes the value of real to the default output channel in
         fixed-point text form, rounded to the given place relative
         to the decimal point, in a field of the given minimum width.
      *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (real: REAL; width: CARDINAL);
      (* Writes the value of real to the default output channel, as
         WriteFixed if the sign and magnitude can be shown in the
         given width, or otherwise as WriteFloat. The number of
         places or significant digits depends on the given width.
      *)
    
    END SRealIO.
    

@c @node gm2-libs-iso/SShortIO, gm2-libs-iso/SShortWholeIO, gm2-libs-iso/SRealIO, M2 ISO Libraries
gm2-libs-iso/SShortIO
---------------------

.. code-block:: modula2
    DEFINITION MODULE SShortIO;

  (* Input and output of short real numbers in decimal text form
     using default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
      (* The text form of a signed fixed-point real number is
           ["+" | "-"], decimal digit, {decimal digit},
           [".", {decimal digit}]
    
         The text form of a signed floating-point real number is
           signed fixed-point real number,
           "E", ["+" | "-"], decimal digit, {decimal digit}
      *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (VAR real: SHORTREAL);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of a signed
         fixed or floating point number. The value of this number
         is assigned to real.  The read result is set to the value
         allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteFloat
.. code-block:: modula2
    PROCEDURE WriteFloat (real: SHORTREAL; sigFigs: CARDINAL;
                          width: CARDINAL);
      (* Writes the value of real to the default output channel in
         floating-point text form, with sigFigs significant figures,
         in a field of the given minimum width.
      *)
    
.. index::
   WriteEng
.. code-block:: modula2
    PROCEDURE WriteEng (real: SHORTREAL; sigFigs: CARDINAL;
                        width: CARDINAL);
      (* As for WriteFloat, except that the number is scaled with
         one to three digits in the whole number part, and with an
         exponent that is a multiple of three.
      *)
    
.. index::
   WriteFixed
.. code-block:: modula2
    PROCEDURE WriteFixed (real: SHORTREAL; place: INTEGER;
                          width: CARDINAL);
      (* Writes the value of real to the default output channel in
         fixed-point text form, rounded to the given place relative
         to the decimal point, in a field of the given minimum width.
      *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (real: SHORTREAL; width: CARDINAL);
      (* Writes the value of real to the default output channel, as
         WriteFixed if the sign and magnitude can be shown in the
         given width, or otherwise as WriteFloat. The number of
         places or significant digits depends on the given width.
      *)
    
    END SShortIO.
    

@c @node gm2-libs-iso/SShortWholeIO, gm2-libs-iso/STextIO, gm2-libs-iso/SShortIO, M2 ISO Libraries
gm2-libs-iso/SShortWholeIO
--------------------------

.. code-block:: modula2
    DEFINITION MODULE SShortWholeIO;

  (* Input and output of whole numbers in decimal text form over
     default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
      (* The text form of a signed whole number is
           ["+" | "-"], decimal digit, {decimal digit}
    
         The text form of an unsigned whole number is
           decimal digit, {decimal digit}
      *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (VAR int: SHORTINT);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of a signed
         whole number.  The value of this number is assigned
         to int.  The read result is set to the value allRight,
         outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (int: SHORTINT; width: CARDINAL);
      (* Writes the value of int to the default output channel in
         text form, in a field of the given minimum width.
      *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (VAR card: SHORTCARD);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of an
         unsigned whole number.  The value of this number is
         assigned to card.  The read result is set to the value
         allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (card: SHORTCARD; width: CARDINAL);
      (* Writes the value of card to the default output channel in
         text form, in a field of the given minimum width.
      *)
    
    END SShortWholeIO.

@c @node gm2-libs-iso/STextIO, gm2-libs-iso/SWholeIO, gm2-libs-iso/SShortWholeIO, M2 ISO Libraries
gm2-libs-iso/STextIO
--------------------

.. code-block:: modula2
    DEFINITION MODULE STextIO;

  (* Input and output of character and string types over default channels. The read result
     is of the type IOConsts.ReadResults.
  *)
    
      (* The following procedures do not read past line marks *)
    
.. index::
   ReadChar
.. code-block:: modula2
    PROCEDURE ReadChar (VAR ch: CHAR);
      (* If possible, removes a character from the default input stream, and assigns the
         corresponding value to ch.  The read result is set to allRight, endOfLine or
         endOfInput.
      *)
    
.. index::
   ReadRestLine
.. code-block:: modula2
    PROCEDURE ReadRestLine (VAR s: ARRAY OF CHAR);
      (* Removes any remaining characters from the default input stream before the next line
         mark, copying to s as many as can be accommodated as a string value.  The read result
         is set to the value allRight, outOfRange, endOfLine, or endOfInput.
      *)
    
.. index::
   ReadString
.. code-block:: modula2
    PROCEDURE ReadString (VAR s: ARRAY OF CHAR);
      (* Removes only those characters from the default input stream before the next line mark
         that can be accommodated in s as a string value, and copies them to s. The read result
         is set to the value allRight, endOfLine, or endOfInput.
      *)
    
.. index::
   ReadToken
.. code-block:: modula2
    PROCEDURE ReadToken (VAR s: ARRAY OF CHAR);
      (* Skips leading spaces, and then removes characters from the default input stream before
         the next space or line mark, copying to s as many as can be accommodated as a string
         value.  The read result is set to the value allRight, outOfRange, endOfLine, or
         endOfInput.
      *)
    
      (* The following procedure reads past the next line mark *)
    
.. index::
   SkipLine
.. code-block:: modula2
    PROCEDURE SkipLine;
      (* Removes successive items from the default input stream up to and including the next
         line mark or until the end of input is reached. The read result is set to the value
         allRight, or endOfInput.
      *)
    
    
      (* Output procedures *)
    
.. index::
   WriteChar
.. code-block:: modula2
    PROCEDURE WriteChar (ch: CHAR);
      (* Writes the value of ch to the default output stream. *)
    
.. index::
   WriteLn
.. code-block:: modula2
    PROCEDURE WriteLn;
      (* Writes a line mark to the default output stream. *)
    
.. index::
   WriteString
.. code-block:: modula2
    PROCEDURE WriteString (s: ARRAY OF CHAR);
      (* Writes the string value of s to the default output stream. *)
    
    END STextIO.

@c @node gm2-libs-iso/SWholeIO, gm2-libs-iso/SYSTEM, gm2-libs-iso/STextIO, M2 ISO Libraries
gm2-libs-iso/SWholeIO
---------------------

.. code-block:: modula2
    DEFINITION MODULE SWholeIO;

  (* Input and output of whole numbers in decimal text form over
     default channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
      (* The text form of a signed whole number is
           ["+" | "-"], decimal digit, {decimal digit}
    
         The text form of an unsigned whole number is
           decimal digit, {decimal digit}
      *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (VAR int: INTEGER);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of a signed
         whole number.  The value of this number is assigned
         to int.  The read result is set to the value allRight,
         outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (int: INTEGER; width: CARDINAL);
      (* Writes the value of int to the default output channel in
         text form, in a field of the given minimum width.
      *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (VAR card: CARDINAL);
      (* Skips leading spaces, and removes any remaining characters
         from the default input channel that form part of an
         unsigned whole number.  The value of this number is
         assigned to card.  The read result is set to the value
         allRight, outOfRange, wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (card: CARDINAL; width: CARDINAL);
      (* Writes the value of card to the default output channel in
         text form, in a field of the given minimum width.
      *)
    
    END SWholeIO.

@c @node gm2-libs-iso/SYSTEM, gm2-libs-iso/Semaphores, gm2-libs-iso/SWholeIO, M2 ISO Libraries
gm2-libs-iso/SYSTEM
-------------------

.. code-block:: modula2
    DEFINITION MODULE SYSTEM;

  (* Gives access to system programming facilities that are probably
     non portable. *)
    
      (* The constants and types define underlying properties of storage *)
    
    EXPORT QUALIFIED BITSPERLOC, LOCSPERWORD,
                     LOC, BYTE, WORD, ADDRESS, CSIZE_T, CSSIZE_T, (* @SYSTEM_DATATYPES@  *)
                     ADDADR, SUBADR, DIFADR, MAKEADR, ADR, ROTATE,
                     SHIFT, CAST, TSIZE,
    
                     (* Internal GM2 compiler functions *)
                     ShiftVal, ShiftLeft, ShiftRight,
                     RotateVal, RotateLeft, RotateRight,
                     THROW, TBITSIZE ;
    
    CONST
                      (* <implementation-defined constant> ; *)
.. index::
   pair: BITSPERLOC; (const)
.. code-block:: modula2
      BITSPERLOC    = __ATTRIBUTE__ __BUILTIN__ ((BITS_PER_UNIT)) ;
                      (* <implementation-defined constant> ; *)
.. index::
   pair: LOCSPERWORD; (const)
.. code-block:: modula2
      LOCSPERWORD   = __ATTRIBUTE__ __BUILTIN__ ((UNITS_PER_WORD)) ;
                      (* <implementation-defined constant> ; *)
.. index::
   pair: LOCSPERBYTE; (const)
.. code-block:: modula2
      LOCSPERBYTE = 8 DIV BITSPERLOC ;
    
    (*
       all the objects below are declared internally to gm2
       ====================================================
    
    TYPE
       @SYSTEM_TYPES@
    
    TYPE
      LOC; (* A system basic type. Values are the uninterpreted
              contents of the smallest addressable unit of storage *)
.. index::
   pair: ADDRESS; (type)
.. code-block:: modula2
      ADDRESS = POINTER TO LOC;
.. index::
   pair: WORD; (type)
.. code-block:: modula2
      WORD = ARRAY [0 .. LOCSPERWORD-1] OF LOC;
    
      (* BYTE and LOCSPERBYTE are provided if appropriate for machine *)
    
    TYPE
.. index::
   pair: BYTE; (type)
.. code-block:: modula2
      BYTE = ARRAY [0 .. LOCSPERBYTE-1] OF LOC;
    
.. index::
   ADDADR
.. code-block:: modula2
    PROCEDURE ADDADR (addr: ADDRESS; offset: CARDINAL): ADDRESS;
      (* Returns address given by (addr + offset), or may raise
         an exception if this address is not valid.
      *)
    
.. index::
   SUBADR
.. code-block:: modula2
    PROCEDURE SUBADR (addr: ADDRESS; offset: CARDINAL): ADDRESS;
      (* Returns address given by (addr - offset), or may raise an
         exception if this address is not valid.
      *)
    
.. index::
   DIFADR
.. code-block:: modula2
    PROCEDURE DIFADR (addr1, addr2: ADDRESS): INTEGER;
      (* Returns the difference between addresses (addr1 - addr2),
         or may raise an exception if the arguments are invalid
         or address space is non-contiguous.
      *)
    
.. index::
   MAKEADR
.. code-block:: modula2
    PROCEDURE MAKEADR (high: <some type>; ...): ADDRESS;
      (* Returns an address constructed from a list of values whose
         types are implementation-defined, or may raise an
         exception if this address is not valid.
    
         In GNU Modula-2, MAKEADR can take any number of arguments
         which are mapped onto the type ADDRESS. The first parameter
         maps onto the high address bits and subsequent parameters map
         onto lower address bits. For example:
    
         a := MAKEADR(BYTE(0FEH), BYTE(0DCH), BYTE(0BAH), BYTE(098H),
                      BYTE(076H), BYTE(054H), BYTE(032H), BYTE(010H)) ;
    
         then the value of, a, on a 64 bit machine is: 0FEDCBA9876543210H
    
         The parameters do not have to be the same type, but constants
         _must_ be typed.
      *)
    
.. index::
   ADR
.. code-block:: modula2
    PROCEDURE ADR (VAR v: <anytype>): ADDRESS;
      (* Returns the address of variable v. *)
    
.. index::
   ROTATE
.. code-block:: modula2
    PROCEDURE ROTATE (val: <a packedset type>;
                      num: INTEGER): <type of first parameter>;
      (* Returns a bit sequence obtained from val by rotating up/right
         or down/right by the absolute value of num.  The direction is
         down/right if the sign of num is negative, otherwise the direction
         is up/left.
      *)
    
.. index::
   SHIFT
.. code-block:: modula2
    PROCEDURE SHIFT (val: <a packedset type>;
                     num: INTEGER): <type of first parameter>;
      (* Returns a bit sequence obtained from val by shifting up/left
         or down/right by the absolute value of num, introducing
         zeros as necessary.  The direction is down/right if the sign of
         num is negative, otherwise the direction is up/left.
      *)
    
.. index::
   CAST
.. code-block:: modula2
    PROCEDURE CAST (<targettype>; val: <anytype>): <targettype>;
      (* CAST is a type transfer function.  Given the expression
         denoted by val, it returns a value of the type <targettype>.
         An invalid value for the target value or a
         physical address alignment problem may raise an exception.
      *)
    
.. index::
   TSIZE
.. code-block:: modula2
    PROCEDURE TSIZE (<type>; ... ): CARDINAL;
      (* Returns the number of LOCS used to store a value of the
         specified <type>.   The extra parameters, if present,
         are used to distinguish variants in a variant record.
      *)
    
.. index::
   THROW
.. code-block:: modula2
    PROCEDURE THROW (i: INTEGER) ;
      (*
         THROW is a GNU extension and was not part of the PIM or ISO
         standards.  It throws an exception which will be caught by the
         EXCEPT block (assuming it exists).  This is a compiler builtin
         function which interfaces to the GCC exception handling runtime
         system.
         GCC uses the term throw, hence the naming distinction between
         the GCC builtin and the Modula-2 runtime library procedure Raise.
         The later library procedure Raise will call SYSTEM.THROW after
         performing various housekeeping activities.
      *)
    
.. index::
   TBITSIZE
.. code-block:: modula2
    PROCEDURE TBITSIZE (<type>) : CARDINAL ;
      (* Returns the minimum number of bits necessary to represent
         <type>.  This procedure function is only useful for determining
         the number of bits used for any type field within a packed RECORD.
         It is not particularly useful elsewhere since <type> might be
         optimized for speed, for example a BOOLEAN could occupy a WORD.
      *)
    *)
    
    
    (* The following procedures are invoked by GNU Modula-2 to
       shift non word set types. They are not part of ISO Modula-2
       but are used to implement the SHIFT procedure defined above. *)
    
    (*
       ShiftVal - is a runtime procedure whose job is to implement
                  the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
                  inline a SHIFT of a single WORD sized set and will only
                  call this routine for larger sets.
    *)
    
.. index::
   ShiftVal
.. code-block:: modula2
    PROCEDURE ShiftVal (VAR s, d: ARRAY OF BITSET;
                        SetSizeInBits: CARDINAL;
                        ShiftCount: INTEGER) ;
    
    
    (*
       ShiftLeft - performs the shift left for a multi word set.
                   This procedure might be called by the back end of
                   GNU Modula-2 depending whether amount is known at
                   compile time.
    *)
    
.. index::
   ShiftLeft
.. code-block:: modula2
    PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         ShiftCount: CARDINAL) ;
    
    (*
       ShiftRight - performs the shift left for a multi word set.
                    This procedure might be called by the back end of
                    GNU Modula-2 depending whether amount is known at
                    compile time.
    *)
    
.. index::
   ShiftRight
.. code-block:: modula2
    PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         ShiftCount: CARDINAL) ;
    
    
    (*
       RotateVal - is a runtime procedure whose job is to implement
                   the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
                   inline a ROTATE of a single WORD (or less)
                   sized set and will only call this routine for larger
                   sets.
    *)
    
.. index::
   RotateVal
.. code-block:: modula2
    PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                         SetSizeInBits: CARDINAL;
                         RotateCount: INTEGER) ;
    
    
    (*
       RotateLeft - performs the rotate left for a multi word set.
                    This procedure might be called by the back end of
                    GNU Modula-2 depending whether amount is known at
                    compile time.
    *)
    
.. index::
   RotateLeft
.. code-block:: modula2
    PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                          SetSizeInBits: CARDINAL;
                          RotateCount: CARDINAL) ;
    
    
    (*
       RotateRight - performs the rotate right for a multi word set.
                     This procedure might be called by the back end of
                     GNU Modula-2 depending whether amount is known at
                     compile time.
    *)
    
.. index::
   RotateRight
.. code-block:: modula2
    PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                           SetSizeInBits: CARDINAL;
                           RotateCount: CARDINAL) ;
    
    
    END SYSTEM.

@c @node gm2-libs-iso/Semaphores, gm2-libs-iso/SeqFile, gm2-libs-iso/SYSTEM, M2 ISO Libraries
gm2-libs-iso/Semaphores
-----------------------

.. code-block:: modula2
    DEFINITION MODULE Semaphores;

  (* Provides mutual exclusion facilities for use by processes. *)
    
    TYPE
      SEMAPHORE;
    
.. index::
   Create
.. code-block:: modula2
    PROCEDURE Create (VAR s: SEMAPHORE; initialCount: CARDINAL );
      (* Creates and returns s as the identity of a new semaphore that
         has its associated count initialized to initialCount, and has
         no processes yet waiting on it.
      *)
    
.. index::
   Destroy
.. code-block:: modula2
    PROCEDURE Destroy (VAR s: SEMAPHORE);
      (* Recovers the resources used to implement the semaphore s,
         provided that no process is waiting for s to become free.
      *)
    
.. index::
   Claim
.. code-block:: modula2
    PROCEDURE Claim (s: SEMAPHORE);
      (* If the count associated with the semaphore s is non-zero,
         decrements this count and allows the calling process to
         continue; otherwise suspends the calling process until
         s is released.
      *)
    
.. index::
   Release
.. code-block:: modula2
    PROCEDURE Release (s: SEMAPHORE);
      (* If there are any processes waiting on the semaphore s,
         allows one of them to enter the ready state; otherwise
         increments the count associated with s.
      *)
    
.. index::
   CondClaim
.. code-block:: modula2
    PROCEDURE CondClaim (s: SEMAPHORE): BOOLEAN;
      (* Returns FALSE if the call Claim(s) would cause the calling
         process to be suspended; in this case the count associated
         with s is not changed. Otherwise returns TRUE and the
         associated count is decremented.
      *)
    
    END Semaphores.
    

@c @node gm2-libs-iso/SeqFile, gm2-libs-iso/ShortComplexMath, gm2-libs-iso/Semaphores, M2 ISO Libraries
gm2-libs-iso/SeqFile
--------------------

.. code-block:: modula2
    DEFINITION MODULE SeqFile;

  (* Rewindable sequential files *)
    
    IMPORT IOChan, ChanConsts;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
      ChanId = IOChan.ChanId;
.. index::
   pair: FlagSet; (type)
.. code-block:: modula2
      FlagSet = ChanConsts.FlagSet;
.. index::
   pair: OpenResults; (type)
.. code-block:: modula2
      OpenResults = ChanConsts.OpenResults;
    
      (* Accepted singleton values of FlagSet *)
    
    CONST
      (* input operations are requested/available *)
.. index::
   pair: read; (const)
.. code-block:: modula2
      read = FlagSet{ChanConsts.readFlag};
    
      (* output operations are requested/available *)
.. index::
   pair: write; (const)
.. code-block:: modula2
      write = FlagSet{ChanConsts.writeFlag};
    
      (* a file may/must/did exist before the channel is opened *)
.. index::
   pair: old; (const)
.. code-block:: modula2
      old = FlagSet{ChanConsts.oldFlag};
    
      (* text operations are requested/available *)
.. index::
   pair: text; (const)
.. code-block:: modula2
      text = FlagSet{ChanConsts.textFlag};
    
      (* raw operations are requested/available *)
.. index::
   pair: raw; (const)
.. code-block:: modula2
      raw = FlagSet{ChanConsts.rawFlag};
    
.. index::
   OpenWrite
.. code-block:: modula2
    PROCEDURE OpenWrite (VAR cid: ChanId; name: ARRAY OF CHAR;
                         flags: FlagSet; VAR res: OpenResults);
      (*
         Attempts to obtain and open a channel connected to a stored
         rewindable file of the given name.
         The write flag is implied; without the raw flag, text is
         implied.  If successful, assigns to cid the identity of
         the opened channel, assigns the value opened to res, and
         selects output mode, with the write position at the start
         of the file (i.e. the file is of zero length).
         If a channel cannot be opened as required, the value of
         res indicates the reason, and cid identifies the invalid
         channel.
      *)
    
.. index::
   OpenAppend
.. code-block:: modula2
    PROCEDURE OpenAppend (VAR cid: ChanId; name: ARRAY OF CHAR;
                          flags: FlagSet; VAR res: OpenResults);
      (*
         Attempts to obtain and open a channel connected to a stored
         rewindable file of the given name.  The write and old flags
         are implied; without the raw flag, text is implied.  If
         successful, assigns to cid the identity of the opened channel,
         assigns the value opened to res, and selects output mode,
         with the write position corresponding to the length of the
         file.  If a channel cannot be opened as required, the value
         of res indicates the reason, and cid identifies the invalid
         channel.
      *)
    
.. index::
   OpenRead
.. code-block:: modula2
    PROCEDURE OpenRead (VAR cid: ChanId; name: ARRAY OF CHAR;
                        flags: FlagSet; VAR res: OpenResults);
      (* Attempts to obtain and open a channel connected to a stored
         rewindable file of the given name.
         The read and old flags are implied; without the raw flag,
         text is implied.  If successful, assigns to cid the
         identity of the opened channel, assigns the value opened to
         res, and selects input mode, with the read position
         corresponding to the start of the file.
         If a channel cannot be opened as required, the value of
         res indicates the reason, and cid identifies the invalid
         channel.
      *)
    
.. index::
   IsSeqFile
.. code-block:: modula2
    PROCEDURE IsSeqFile (cid: ChanId): BOOLEAN;
      (* Tests if the channel identified by cid is open to a
         rewindable sequential file. *)
    
.. index::
   Reread
.. code-block:: modula2
    PROCEDURE Reread (cid: ChanId);
      (* If the channel identified by cid is not open to a rewindable
         sequential file, the exception wrongDevice is raised;
         otherwise attempts to set the read position to the
         start of the file, and to select input mode.
         If the operation cannot be performed (perhaps because of
         insufficient permissions) neither input mode nor output
         mode is selected.
      *)
    
.. index::
   Rewrite
.. code-block:: modula2
    PROCEDURE Rewrite (cid: ChanId);
      (* If the channel identified by cid is not open to a
         rewindable sequential file, the exception wrongDevice is
         raised; otherwise, attempts to truncate the file to zero
         length, and to select output mode.  If the operation
         cannot be performed (perhaps because of insufficient
         permissions) neither input mode nor output mode is selected.
      *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (VAR cid: ChanId);
      (* If the channel identified by cid is not open to a rewindable
         sequential file, the exception wrongDevice is raised;
         otherwise closes the channel, and assigns the value
         identifying the invalid channel to cid.
      *)
    
    END SeqFile.
    

@c @node gm2-libs-iso/ShortComplexMath, gm2-libs-iso/ShortIO, gm2-libs-iso/SeqFile, M2 ISO Libraries
gm2-libs-iso/ShortComplexMath
-----------------------------

.. code-block:: modula2
    DEFINITION MODULE ShortComplexMath;

  (* Mathematical functions for the type SHORTCOMPLEX *)
    
    CONST
.. index::
   pair: i; (const)
.. code-block:: modula2
      i =    CMPLX (0.0, 1.0);
.. index::
   pair: one; (const)
.. code-block:: modula2
      one =  CMPLX (1.0, 0.0);
.. index::
   pair: zero; (const)
.. code-block:: modula2
      zero = CMPLX (0.0, 0.0);
    
.. index::
   abs
.. code-block:: modula2
    PROCEDURE abs (z: SHORTCOMPLEX): SHORTREAL;
      (* Returns the length of z *)
    
.. index::
   arg
.. code-block:: modula2
    PROCEDURE arg (z: SHORTCOMPLEX): SHORTREAL;
      (* Returns the angle that z subtends to the positive real axis *)
    
.. index::
   conj
.. code-block:: modula2
    PROCEDURE conj (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the complex conjugate of z *)
    
.. index::
   power
.. code-block:: modula2
    PROCEDURE power (base: SHORTCOMPLEX; exponent: SHORTREAL): SHORTCOMPLEX;
      (* Returns the value of the number base raised to the power exponent *)
    
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE sqrt (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the principal square root of z *)
    
.. index::
   exp
.. code-block:: modula2
    PROCEDURE exp (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the complex exponential of z *)
    
.. index::
   ln
.. code-block:: modula2
    PROCEDURE ln (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the principal value of the natural logarithm of z *)
    
.. index::
   sin
.. code-block:: modula2
    PROCEDURE sin (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the sine of z *)
    
.. index::
   cos
.. code-block:: modula2
    PROCEDURE cos (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the cosine of z *)
    
.. index::
   tan
.. code-block:: modula2
    PROCEDURE tan (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the tangent of z *)
    
.. index::
   arcsin
.. code-block:: modula2
    PROCEDURE arcsin (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the arcsine of z *)
    
.. index::
   arccos
.. code-block:: modula2
    PROCEDURE arccos (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the arccosine of z *)
    
.. index::
   arctan
.. code-block:: modula2
    PROCEDURE arctan (z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the arctangent of z *)
    
.. index::
   polarToComplex
.. code-block:: modula2
    PROCEDURE polarToComplex (abs, arg: SHORTREAL): SHORTCOMPLEX;
      (* Returns the complex number with the specified polar coordinates *)
    
.. index::
   scalarMult
.. code-block:: modula2
    PROCEDURE scalarMult (scalar: SHORTREAL; z: SHORTCOMPLEX): SHORTCOMPLEX;
      (* Returns the scalar product of scalar with z *)
    
.. index::
   IsCMathException
.. code-block:: modula2
    PROCEDURE IsCMathException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution state
         because of the raising of an exception in a routine from this module; otherwise
         returns FALSE.
      *)
    
    END ShortComplexMath.
    

@c @node gm2-libs-iso/ShortIO, gm2-libs-iso/ShortWholeIO, gm2-libs-iso/ShortComplexMath, M2 ISO Libraries
gm2-libs-iso/ShortIO
--------------------

.. code-block:: modula2
    DEFINITION MODULE ShortIO;

  (* Input and output of short real numbers in decimal text form
     over specified channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The text form of a signed fixed-point real number is
           ["+" | "-"], decimal digit, {decimal digit}, [".",
           {decimal digit}]
    
         The text form of a signed floating-point real number is
           signed fixed-point real number,
           "E", ["+" | "-"], decimal digit, {decimal digit}
      *)
    
.. index::
   ReadReal
.. code-block:: modula2
    PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: SHORTREAL);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of a signed fixed or floating
         point number.  The value of this number is assigned to real.
         The read result is set to the value allRight, outOfRange,
         wrongFormat, endOfLine, or endOfInput.
      *)
    
.. index::
   WriteFloat
.. code-block:: modula2
    PROCEDURE WriteFloat (cid: IOChan.ChanId; real: SHORTREAL;
                          sigFigs: CARDINAL; width: CARDINAL);
      (* Writes the value of real to cid in floating-point text form,
         with sigFigs significant figures, in a field of the given
         minimum width.
      *)
    
.. index::
   WriteEng
.. code-block:: modula2
    PROCEDURE WriteEng (cid: IOChan.ChanId; real: SHORTREAL;
                        sigFigs: CARDINAL; width: CARDINAL);
      (* As for WriteFloat, except that the number is scaled with
         one to three digits in the whole number part, and with an
         exponent that is a multiple of three.
      *)
    
.. index::
   WriteFixed
.. code-block:: modula2
    PROCEDURE WriteFixed (cid: IOChan.ChanId; real: SHORTREAL;
                          place: INTEGER; width: CARDINAL);
      (* Writes the value of real to cid in fixed-point text form,
         rounded to the given place relative to the decimal point,
         in a field of the given minimum width.
      *)
    
.. index::
   WriteReal
.. code-block:: modula2
    PROCEDURE WriteReal (cid: IOChan.ChanId; real: SHORTREAL;
                         width: CARDINAL);
      (* Writes the value of real to cid, as WriteFixed if the
         sign and magnitude can be shown in the given width, or
         otherwise as WriteFloat.  The number of places or
         significant digits depends on the given width.
      *)
    
    END ShortIO.

@c @node gm2-libs-iso/ShortWholeIO, gm2-libs-iso/SimpleCipher, gm2-libs-iso/ShortIO, M2 ISO Libraries
gm2-libs-iso/ShortWholeIO
-------------------------

.. code-block:: modula2
    DEFINITION MODULE ShortWholeIO;

  (* Input and output of whole numbers in decimal text form
     over specified channels.  The read result is of the
     type IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The text form of a signed whole number is
           ["+" | "-"], decimal digit, {decimal digit}
    
         The text form of an unsigned whole number is
           decimal digit, {decimal digit}
      *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (cid: IOChan.ChanId; VAR int: SHORTINT);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of a signed whole number.  The
         value of this number is assigned to int.  The read result
         is set to the value allRight, outOfRange, wrongFormat,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (cid: IOChan.ChanId; int: SHORTINT;
                        width: CARDINAL);
      (* Writes the value of int to cid in text form, in a field of
         the given minimum width. *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (cid: IOChan.ChanId; VAR card: SHORTCARD);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of an unsigned whole number.  The
         value of this number is assigned to card. The read result
         is set to the value allRight, outOfRange, wrongFormat,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (cid: IOChan.ChanId; card: SHORTCARD;
                         width: CARDINAL);
      (* Writes the value of card to cid in text form, in a field
         of the given minimum width. *)
    
    END ShortWholeIO.

@c @node gm2-libs-iso/SimpleCipher, gm2-libs-iso/StdChans, gm2-libs-iso/ShortWholeIO, M2 ISO Libraries
gm2-libs-iso/SimpleCipher
-------------------------

.. code-block:: modula2
    DEFINITION MODULE SimpleCipher ;

(*
    Description: provides a simple Caesar cipher layer which
                 can be attached to any channel device.  This,
                 pedagogical, module is designed to show how
                 it is possible to add further layers underneath
                 the channel devices.
*)
    
    FROM IOChan IMPORT ChanId ;
    
    
    (*
       InsertCipherLayer - inserts a caesar cipher below channel, cid.
                           The encryption, key, is specified.
    *)
    
.. index::
   InsertCipherLayer
.. code-block:: modula2
    PROCEDURE InsertCipherLayer (cid: ChanId; key: INTEGER) ;
    
    
    (*
       RemoveCipherLayer - removes a Caesar cipher below channel, cid.
    *)
    
.. index::
   RemoveCipherLayer
.. code-block:: modula2
    PROCEDURE RemoveCipherLayer (cid: ChanId) ;
    
    
    END SimpleCipher.

@c @node gm2-libs-iso/StdChans, gm2-libs-iso/Storage, gm2-libs-iso/SimpleCipher, M2 ISO Libraries
gm2-libs-iso/StdChans
---------------------

.. code-block:: modula2
    DEFINITION MODULE StdChans;

  (* Access to standard and default channels *)
    
    IMPORT IOChan;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
      ChanId = IOChan.ChanId;
        (* Values of this type are used to identify channels *)
    
      (* The following functions return the standard channel values.
         These channels cannot be closed.
      *)
    
.. index::
   StdInChan
.. code-block:: modula2
    PROCEDURE StdInChan (): ChanId;
      (* Returns the identity of the implementation-defined standard source for
    program
         input.
      *)
    
.. index::
   StdOutChan
.. code-block:: modula2
    PROCEDURE StdOutChan (): ChanId;
      (* Returns the identity of the implementation-defined standard source for program
         output.
      *)
    
.. index::
   StdErrChan
.. code-block:: modula2
    PROCEDURE StdErrChan (): ChanId;
      (* Returns the identity of the implementation-defined standard destination for program
         error messages.
      *)
    
.. index::
   NullChan
.. code-block:: modula2
    PROCEDURE NullChan (): ChanId;
      (* Returns the identity of a channel open to the null device. *)
    
      (* The following functions return the default channel values *)
    
.. index::
   InChan
.. code-block:: modula2
    PROCEDURE InChan (): ChanId;
      (* Returns the identity of the current default input channel. *)
    
.. index::
   OutChan
.. code-block:: modula2
    PROCEDURE OutChan (): ChanId;
      (* Returns the identity of the current default output channel. *)
    
.. index::
   ErrChan
.. code-block:: modula2
    PROCEDURE ErrChan (): ChanId;
      (* Returns the identity of the current default error message channel. *)
    
      (* The following procedures allow for redirection of the default channels *)
    
.. index::
   SetInChan
.. code-block:: modula2
    PROCEDURE SetInChan (cid: ChanId);
      (* Sets the current default input channel to that identified by cid. *)
    
.. index::
   SetOutChan
.. code-block:: modula2
    PROCEDURE SetOutChan (cid: ChanId);
      (* Sets the current default output channel to that identified by cid. *)
    
.. index::
   SetErrChan
.. code-block:: modula2
    PROCEDURE SetErrChan (cid: ChanId);
      (* Sets the current default error channel to that identified by cid. *)
    
    END StdChans.

@c @node gm2-libs-iso/Storage, gm2-libs-iso/StreamFile, gm2-libs-iso/StdChans, M2 ISO Libraries
gm2-libs-iso/Storage
--------------------

.. code-block:: modula2
    DEFINITION MODULE Storage;

  (* Facilities for dynamically allocating and deallocating storage *)
    
    IMPORT SYSTEM;
    
.. index::
   ALLOCATE
.. code-block:: modula2
    PROCEDURE ALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
      (* Allocates storage for a variable of size amount and assigns
         the address of this variable to addr. If there is insufficient
         unallocated storage to do this, the value NIL is assigned to addr.
      *)
    
.. index::
   DEALLOCATE
.. code-block:: modula2
    PROCEDURE DEALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
      (* Deallocates amount locations allocated by ALLOCATE for
         the storage of the variable addressed by addr and assigns
         the value NIL to addr.
      *)
    
.. index::
   REALLOCATE
.. code-block:: modula2
    PROCEDURE REALLOCATE (VAR addr: SYSTEM.ADDRESS; amount: CARDINAL);
      (* Attempts to reallocate, amount of storage.  Effectively it
         calls ALLOCATE, copies the amount of data pointed to by
         addr into the new space and DEALLOCATES the addr.
         This procedure is a GNU extension.
      *)
    
    TYPE
.. index::
   pair: StorageExceptions; (type)
.. code-block:: modula2
      StorageExceptions = (
        nilDeallocation,             (* first argument to DEALLOCATE is NIL *)
        pointerToUnallocatedStorage, (* storage to deallocate not allocated by ALLOCATE *)
        wrongStorageToUnallocate     (* amount to deallocate is not amount allocated *)
      );
    
.. index::
   IsStorageException
.. code-block:: modula2
    PROCEDURE IsStorageException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional
         execution state because of the raising of an exception from
         StorageExceptions; otherwise returns FALSE.
      *)
    
.. index::
   StorageException
.. code-block:: modula2
    PROCEDURE StorageException (): StorageExceptions;
      (* If the current coroutine is in the exceptional execution
         state because of the raising of an exception from
         StorageExceptions, returns the corresponding
         enumeration value, and otherwise raises an exception.
      *)
    
    END Storage.

@c @node gm2-libs-iso/StreamFile, gm2-libs-iso/StringChan, gm2-libs-iso/Storage, M2 ISO Libraries
gm2-libs-iso/StreamFile
-----------------------

.. code-block:: modula2
    DEFINITION MODULE StreamFile;

  (* Independent sequential data streams *)
    
    IMPORT IOChan, ChanConsts;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
      ChanId = IOChan.ChanId;
.. index::
   pair: FlagSet; (type)
.. code-block:: modula2
      FlagSet = ChanConsts.FlagSet;
.. index::
   pair: OpenResults; (type)
.. code-block:: modula2
      OpenResults = ChanConsts.OpenResults;
    
      (* Accepted singleton values of FlagSet *)
    
    CONST
.. index::
   pair: read; (const)
.. code-block:: modula2
      read = FlagSet{ChanConsts.readFlag};   (* input operations are requested/available *)
.. index::
   pair: write; (const)
.. code-block:: modula2
      write = FlagSet{ChanConsts.writeFlag}; (* output operations are requested/available *)
.. index::
   pair: old; (const)
.. code-block:: modula2
      old = FlagSet{ChanConsts.oldFlag};     (* a file may/must/did exist before the channel is
                                                opened *)
.. index::
   pair: text; (const)
.. code-block:: modula2
      text = FlagSet{ChanConsts.textFlag};   (* text operations are requested/available *)
.. index::
   pair: raw; (const)
.. code-block:: modula2
      raw = FlagSet{ChanConsts.rawFlag};     (* raw operations are requested/available *)
    
    
.. index::
   Open
.. code-block:: modula2
    PROCEDURE Open (VAR cid: ChanId; name: ARRAY OF CHAR;
                    flags: FlagSet; VAR res: OpenResults);
      (* Attempts to obtain and open a channel connected to a
         sequential stream of the given name.
         The read flag implies old; without the raw flag, text is
         implied.  If successful, assigns to cid the identity of
         the opened channel, and assigns the value opened to res.
         If a channel cannot be opened as required, the value of
         res indicates the reason, and cid identifies the invalid
         channel.
      *)
    
.. index::
   IsStreamFile
.. code-block:: modula2
    PROCEDURE IsStreamFile (cid: ChanId): BOOLEAN;
      (* Tests if the channel identified by cid is open to a sequential stream. *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (VAR cid: ChanId);
      (* If the channel identified by cid is not open to a sequential stream, the exception
         wrongDevice is raised; otherwise closes the channel, and assigns the value identifying
         the invalid channel to cid.
      *)
    
    END StreamFile.
    

@c @node gm2-libs-iso/StringChan, gm2-libs-iso/Strings, gm2-libs-iso/StreamFile, M2 ISO Libraries
gm2-libs-iso/StringChan
-----------------------

.. code-block:: modula2
    DEFINITION MODULE StringChan ;

(*
    Description: provides a set of Channel and String
                 input and output procedures.
*)
    
    FROM DynamicStrings IMPORT String ;
    IMPORT IOChan;
    
    
    (*
       writeString - writes a string, s, to ChanId, cid.
                     The string, s, is not destroyed.
    *)
    
.. index::
   writeString
.. code-block:: modula2
    PROCEDURE writeString (cid: IOChan.ChanId; s: String) ;
    
    
    (*
       writeFieldWidth - writes a string, s, to ChanId, cid.
                         The string, s, is not destroyed and it
                         is prefixed by spaces so that at least,
                         width, characters are written.  If the
                         string, s, is longer than width then
                         no spaces are prefixed to the output
                         and the entire string is written.
    *)
    
.. index::
   writeFieldWidth
.. code-block:: modula2
    PROCEDURE writeFieldWidth (cid: IOChan.ChanId;
                               s: String; width: CARDINAL) ;
    
    
    END StringChan.

@c @node gm2-libs-iso/Strings, gm2-libs-iso/SysClock, gm2-libs-iso/StringChan, M2 ISO Libraries
gm2-libs-iso/Strings
--------------------

.. code-block:: modula2
    DEFINITION MODULE Strings;

  (* Facilities for manipulating strings *)
    
    TYPE
.. index::
   pair: String1; (type)
.. code-block:: modula2
      String1 = ARRAY [0..0] OF CHAR;
        (* String1 is provided for constructing a value of a single-character string type from a
           single character value in order to pass CHAR values to ARRAY OF CHAR parameters.
        *)
    
.. index::
   Length
.. code-block:: modula2
    PROCEDURE Length (stringVal: ARRAY OF CHAR): CARDINAL;
      (* Returns the length of stringVal (the same value as would be returned by the
         pervasive function LENGTH).
      *)
    
    
    (* The following seven procedures construct a string value, and attempt to assign it to a
       variable parameter.  They all have the property that if the length of the constructed string
       value exceeds the capacity of the variable parameter, a truncated value is assigned, while
       if the length of the constructed string value is less than the capacity of the variable
       parameter, a string terminator is appended before assignment is performed.
    *)
    
.. index::
   Assign
.. code-block:: modula2
    PROCEDURE Assign (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
      (* Copies source to destination *)
    
.. index::
   Extract
.. code-block:: modula2
    PROCEDURE Extract (source: ARRAY OF CHAR; startIndex, numberToExtract: CARDINAL;
                       VAR destination: ARRAY OF CHAR);
      (* Copies at most numberToExtract characters from source to destination, starting at position
         startIndex in source.
      *)
    
.. index::
   Delete
.. code-block:: modula2
    PROCEDURE Delete (VAR stringVar: ARRAY OF CHAR; startIndex, numberToDelete:
    CARDINAL);
      (* Deletes at most numberToDelete characters from stringVar, starting at position
         startIndex.
      *)
    
.. index::
   Insert
.. code-block:: modula2
    PROCEDURE Insert (source: ARRAY OF CHAR; startIndex: CARDINAL;
                      VAR destination: ARRAY OF CHAR);
      (* Inserts source into destination at position startIndex *)
    
.. index::
   Replace
.. code-block:: modula2
    PROCEDURE Replace (source: ARRAY OF CHAR; startIndex: CARDINAL;
                       VAR destination: ARRAY OF CHAR);
      (* Copies source into destination, starting at position startIndex. Copying stops when
         all of source has been copied, or when the last character of the string value in
         destination has been replaced.
      *)
    
.. index::
   Append
.. code-block:: modula2
    PROCEDURE Append (source: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
      (* Appends source to destination. *)
    
.. index::
   Concat
.. code-block:: modula2
    PROCEDURE Concat (source1, source2: ARRAY OF CHAR; VAR destination: ARRAY OF CHAR);
      (* Concatenates source2 onto source1 and copies the result into destination. *)
    
    (* The following predicates provide for pre-testing of the operation-completion
       conditions for the procedures above.
    *)
    
.. index::
   CanAssignAll
.. code-block:: modula2
    PROCEDURE CanAssignAll (sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR): BOOLEAN;
      (* Returns TRUE if a number of characters, indicated by sourceLength, will fit into
         destination; otherwise returns FALSE.
      *)
    
.. index::
   CanExtractAll
.. code-block:: modula2
    PROCEDURE CanExtractAll (sourceLength, startIndex, numberToExtract: CARDINAL;
                             VAR destination: ARRAY OF CHAR): BOOLEAN;
      (* Returns TRUE if there are numberToExtract characters starting at startIndex and
         within the sourceLength of some string, and if the capacity of destination is
         sufficient to hold numberToExtract characters; otherwise returns FALSE.
      *)
    
.. index::
   CanDeleteAll
.. code-block:: modula2
    PROCEDURE CanDeleteAll (stringLength, startIndex, numberToDelete: CARDINAL): BOOLEAN;
      (* Returns TRUE if there are numberToDelete characters starting at startIndex and
         within the stringLength of some string; otherwise returns FALSE.
      *)
    
.. index::
   CanInsertAll
.. code-block:: modula2
    PROCEDURE CanInsertAll (sourceLength, startIndex: CARDINAL;
                            VAR destination: ARRAY OF CHAR): BOOLEAN;
      (* Returns TRUE if there is room for the insertion of sourceLength characters from
         some string into destination starting at startIndex; otherwise returns FALSE.
      *)
    
.. index::
   CanReplaceAll
.. code-block:: modula2
    PROCEDURE CanReplaceAll (sourceLength, startIndex: CARDINAL;
                             VAR destination: ARRAY OF CHAR): BOOLEAN;
      (* Returns TRUE if there is room for the replacement of sourceLength characters in
         destination starting at startIndex; otherwise returns FALSE.
      *)
    
.. index::
   CanAppendAll
.. code-block:: modula2
    PROCEDURE CanAppendAll (sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR): BOOLEAN;
      (* Returns TRUE if there is sufficient room in destination to append a string of
         length sourceLength to the string in destination; otherwise returns FALSE.
      *)
    
.. index::
   CanConcatAll
.. code-block:: modula2
    PROCEDURE CanConcatAll (source1Length, source2Length: CARDINAL;
                            VAR destination: ARRAY OF CHAR): BOOLEAN;
      (* Returns TRUE if there is sufficient room in destination for a two strings of
         lengths source1Length and source2Length; otherwise returns FALSE.
      *)
    
    (* The following type and procedures provide for the comparison of string values, and for the
       location of substrings within strings.
    *)
    
    TYPE
.. index::
   pair: CompareResults; (type)
.. code-block:: modula2
      CompareResults = (less, equal, greater);
    
.. index::
   Compare
.. code-block:: modula2
    PROCEDURE Compare (stringVal1, stringVal2: ARRAY OF CHAR): CompareResults;
      (* Returns less, equal, or greater, according as stringVal1 is lexically less than,
         equal to, or greater than stringVal2.
      *)
    
.. index::
   Equal
.. code-block:: modula2
    PROCEDURE Equal (stringVal1, stringVal2: ARRAY OF CHAR): BOOLEAN;
      (* Returns Strings.Compare(stringVal1, stringVal2) = Strings.equal *)
    
.. index::
   FindNext
.. code-block:: modula2
    PROCEDURE FindNext (pattern, stringToSearch: ARRAY OF CHAR; startIndex: CARDINAL;
                        VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
      (* Looks forward for next occurrence of pattern in stringToSearch, starting the search at
         position startIndex. If startIndex < LENGTH(stringToSearch) and pattern is found,
         patternFound is returned as TRUE, and posOfPattern contains the start position in
         stringToSearch of pattern. Otherwise patternFound is returned as FALSE, and posOfPattern
         is unchanged.
      *)
    
.. index::
   FindPrev
.. code-block:: modula2
    PROCEDURE FindPrev (pattern, stringToSearch: ARRAY OF CHAR; startIndex: CARDINAL;
                        VAR patternFound: BOOLEAN; VAR posOfPattern: CARDINAL);
      (* Looks backward for the previous occurrence of pattern in stringToSearch and returns the
         position of the first character of the pattern if found. The search for the pattern
         begins at startIndex. If pattern is found, patternFound is returned as TRUE, and
         posOfPattern contains the start position in stringToSearch of pattern in the range
         [0..startIndex]. Otherwise patternFound is returned as FALSE, and posOfPattern is unchanged.
      *)
    
.. index::
   FindDiff
.. code-block:: modula2
    PROCEDURE FindDiff (stringVal1, stringVal2: ARRAY OF CHAR;
                        VAR differenceFound: BOOLEAN; VAR posOfDifference: CARDINAL);
      (* Compares the string values in stringVal1 and stringVal2 for differences. If they
         are equal, differenceFound is returned as FALSE, and TRUE otherwise. If
         differenceFound is TRUE, posOfDifference is set to the position of the first
         difference; otherwise posOfDifference is unchanged.
      *)
    
.. index::
   Capitalize
.. code-block:: modula2
    PROCEDURE Capitalize (VAR stringVar: ARRAY OF CHAR);
      (* Applies the function CAP to each character of the string value in stringVar. *)
    
    
    END Strings.
    

@c @node gm2-libs-iso/SysClock, gm2-libs-iso/TERMINATION, gm2-libs-iso/Strings, M2 ISO Libraries
gm2-libs-iso/SysClock
---------------------

.. code-block:: modula2
    DEFINITION MODULE SysClock;

(* Facilities for accessing a system clock that records the date
   and time of day *)
    
    CONST
.. index::
   pair: maxSecondParts; (const)
.. code-block:: modula2
      maxSecondParts = 1000000 ;
    
    TYPE
.. index::
   pair: Month; (type)
.. code-block:: modula2
      Month    = [1 .. 12];
.. index::
   pair: Day; (type)
.. code-block:: modula2
      Day      = [1 .. 31];
.. index::
   pair: Hour; (type)
.. code-block:: modula2
      Hour     = [0 .. 23];
.. index::
   pair: Min; (type)
.. code-block:: modula2
      Min      = [0 .. 59];
.. index::
   pair: Sec; (type)
.. code-block:: modula2
      Sec      = [0 .. 59];
.. index::
   pair: Fraction; (type)
.. code-block:: modula2
      Fraction = [0 .. maxSecondParts];
.. index::
   pair: UTCDiff; (type)
.. code-block:: modula2
      UTCDiff  = [-780 .. 720];
.. index::
   pair: DateTime; (type)
.. code-block:: modula2
      DateTime =
        RECORD
          year:      CARDINAL;
          month:     Month;
          day:       Day;
          hour:      Hour;
          minute:    Min;
          second:    Sec;
          fractions: Fraction;      (* parts of a second *)
          zone:      UTCDiff;       (* Time zone differential
                                       factor which is the number
                                       of minutes to add to local
                                       time to obtain UTC. *)
          summerTimeFlag: BOOLEAN;  (* Interpretation of flag
                                       depends on local usage. *)
        END;
    
.. index::
   CanGetClock
.. code-block:: modula2
    PROCEDURE CanGetClock(): BOOLEAN;
    (* Tests if the clock can be read *)
    
.. index::
   CanSetClock
.. code-block:: modula2
    PROCEDURE CanSetClock(): BOOLEAN;
    (* Tests if the clock can be set *)
    
.. index::
   IsValidDateTime
.. code-block:: modula2
    PROCEDURE IsValidDateTime(userData: DateTime): BOOLEAN;
    (* Tests if the value of userData is a valid *)
    
.. index::
   GetClock
.. code-block:: modula2
    PROCEDURE GetClock(VAR userData: DateTime);
    (* Assigns local date and time of the day to userData *)
    
.. index::
   SetClock
.. code-block:: modula2
    PROCEDURE SetClock(userData: DateTime);
    (* Sets the system time clock to the given local date and
       time *)
    
    END SysClock.

@c @node gm2-libs-iso/TERMINATION, gm2-libs-iso/TermFile, gm2-libs-iso/SysClock, M2 ISO Libraries
gm2-libs-iso/TERMINATION
------------------------

.. code-block:: modula2
    DEFINITION MODULE TERMINATION;

(* Provides facilities for enquiries concerning the occurrence of termination events. *)
    
.. index::
   IsTerminating
.. code-block:: modula2
    PROCEDURE IsTerminating (): BOOLEAN ;
      (* Returns true if any coroutine has started  program termination and false otherwise. *)
    
.. index::
   HasHalted
.. code-block:: modula2
    PROCEDURE HasHalted (): BOOLEAN ;
      (* Returns true if a call to HALT has been made and false otherwise. *)
    
    END TERMINATION.

@c @node gm2-libs-iso/TermFile, gm2-libs-iso/TextIO, gm2-libs-iso/TERMINATION, M2 ISO Libraries
gm2-libs-iso/TermFile
---------------------

.. code-block:: modula2
    DEFINITION MODULE TermFile;

  (* Access to the terminal device *)
    
      (* Channels opened by this module are connected to a single
         terminal device; typed characters are distributed between
         channels according to the sequence of read requests.
      *)
    
    IMPORT IOChan, ChanConsts;
    
    TYPE
.. index::
   pair: ChanId; (type)
.. code-block:: modula2
      ChanId = IOChan.ChanId;
.. index::
   pair: FlagSet; (type)
.. code-block:: modula2
      FlagSet = ChanConsts.FlagSet;
.. index::
   pair: OpenResults; (type)
.. code-block:: modula2
      OpenResults = ChanConsts.OpenResults;
    
      (* Accepted singleton values of FlagSet *)
    
    CONST
.. index::
   pair: read; (const)
.. code-block:: modula2
      read = FlagSet{ChanConsts.readFlag};
      (* input operations are requested/available *)
.. index::
   pair: write; (const)
.. code-block:: modula2
      write = FlagSet{ChanConsts.writeFlag};
      (* output operations are requested/available *)
.. index::
   pair: text; (const)
.. code-block:: modula2
      text = FlagSet{ChanConsts.textFlag};
      (* text operations are requested/available *)
.. index::
   pair: raw; (const)
.. code-block:: modula2
      raw = FlagSet{ChanConsts.rawFlag};
      (* raw operations are requested/available *)
.. index::
   pair: echo; (const)
.. code-block:: modula2
      echo = FlagSet{ChanConsts.echoFlag};
      (* echoing by interactive device on reading of
         characters from input stream requested/applies
      *)
    
.. index::
   Open
.. code-block:: modula2
    PROCEDURE Open (VAR cid: ChanId; flagset: FlagSet; VAR res: OpenResults);
      (* Attempts to obtain and open a channel connected to
         the terminal.  Without the raw flag, text is implied.
         Without the echo flag, line mode is requested,
         otherwise single character mode is requested.
         If successful, assigns to cid the identity of
         the opened channel, and assigns the value opened to res.
         If a channel cannot be opened as required, the value of
         res indicates the reason, and cid identifies the
         invalid channel.
      *)
    
.. index::
   IsTermFile
.. code-block:: modula2
    PROCEDURE IsTermFile (cid: ChanId): BOOLEAN;
      (* Tests if the channel identified by cid is open to
         the terminal. *)
    
.. index::
   Close
.. code-block:: modula2
    PROCEDURE Close (VAR cid: ChanId);
      (* If the channel identified by cid is not open to the terminal,
         the exception wrongDevice is raised; otherwise closes the
         channel and assigns the value identifying the invalid channel
         to cid.
      *)
    
    END TermFile.
    

@c @node gm2-libs-iso/TextIO, gm2-libs-iso/WholeConv, gm2-libs-iso/TermFile, M2 ISO Libraries
gm2-libs-iso/TextIO
-------------------

.. code-block:: modula2
    DEFINITION MODULE TextIO;

  (* Input and output of character and string types over
     specified channels.  The read result is of the type
     IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The following procedures do not read past line marks *)
    
.. index::
   ReadChar
.. code-block:: modula2
    PROCEDURE ReadChar (cid: IOChan.ChanId; VAR ch: CHAR);
      (* If possible, removes a character from the input stream
         cid and assigns the corresponding value to ch.  The
         read result is set to the value allRight, endOfLine, or
         endOfInput.
      *)
    
.. index::
   ReadRestLine
.. code-block:: modula2
    PROCEDURE ReadRestLine (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
      (* Removes any remaining characters from the input stream
         cid before the next line mark,  copying to s as many as
         can be accommodated as a string value.  The read result is
         set to the value allRight, outOfRange, endOfLine, or
         endOfInput.
      *)
    
.. index::
   ReadString
.. code-block:: modula2
    PROCEDURE ReadString (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
      (* Removes only those characters from the input stream cid
         before the next line mark that can be accommodated in s
         as a string value, and copies them to s.  The read result
         is set to the value allRight, endOfLine, or endOfInput.
      *)
    
.. index::
   ReadToken
.. code-block:: modula2
    PROCEDURE ReadToken (cid: IOChan.ChanId; VAR s: ARRAY OF CHAR);
      (* Skips leading spaces, and then removes characters from
         the input stream cid before the next space or line mark,
         copying to s as many as can be accommodated as a string
         value.  The read result is set to the value allRight,
         outOfRange, endOfLine, or endOfInput.
      *)
    
      (* The following procedure reads past the next line mark *)
    
.. index::
   SkipLine
.. code-block:: modula2
    PROCEDURE SkipLine (cid: IOChan.ChanId);
      (* Removes successive items from the input stream cid up
         to and including the next line mark, or until the end
         of input is reached.  The read result is set to the
         value allRight, or endOfInput.
      *)
    
      (* Output procedures *)
    
.. index::
   WriteChar
.. code-block:: modula2
    PROCEDURE WriteChar (cid: IOChan.ChanId; ch: CHAR);
      (* Writes the value of ch to the output stream cid. *)
    
.. index::
   WriteLn
.. code-block:: modula2
    PROCEDURE WriteLn (cid: IOChan.ChanId);
      (* Writes a line mark to the output stream cid. *)
    
.. index::
   WriteString
.. code-block:: modula2
    PROCEDURE WriteString (cid: IOChan.ChanId; s: ARRAY OF CHAR);
      (* Writes the string value in s to the output stream cid. *)
    
    END TextIO.
    

@c @node gm2-libs-iso/WholeConv, gm2-libs-iso/WholeIO, gm2-libs-iso/TextIO, M2 ISO Libraries
gm2-libs-iso/WholeConv
----------------------

.. code-block:: modula2
    DEFINITION MODULE WholeConv;

  (* Low-level whole-number/string conversions *)
    
    IMPORT
      ConvTypes;
    
    TYPE
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
      ConvResults = ConvTypes.ConvResults;
            (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
    
.. index::
   ScanInt
.. code-block:: modula2
    PROCEDURE ScanInt (inputCh: CHAR;
                       VAR chClass: ConvTypes.ScanClass;
                       VAR nextState: ConvTypes.ScanState) ;
      (* Represents the start state of a finite state scanner for signed
         whole numbers - assigns class of inputCh to chClass and a
         procedure representing the next state to nextState.
      *)
    
.. index::
   FormatInt
.. code-block:: modula2
    PROCEDURE FormatInt (str: ARRAY OF CHAR): ConvResults;
      (* Returns the format of the string value for conversion to INTEGER. *)
    
.. index::
   ValueInt
.. code-block:: modula2
    PROCEDURE ValueInt (str: ARRAY OF CHAR): INTEGER;
      (* Returns the value corresponding to the signed whole number string
         value str if str is well-formed; otherwise raises the WholeConv
         exception.
       *)
    
.. index::
   LengthInt
.. code-block:: modula2
    PROCEDURE LengthInt (int: INTEGER): CARDINAL;
      (* Returns the number of characters in the string representation of
         int.
       *)
    
.. index::
   ScanCard
.. code-block:: modula2
    PROCEDURE ScanCard (inputCh: CHAR; VAR chClass: ConvTypes.ScanClass;
                        VAR nextState: ConvTypes.ScanState);
      (* Represents the start state of a finite state scanner for unsigned
         whole numbers - assigns class of inputCh to chClass and a procedure
         representing the next state to nextState.
       *)
    
.. index::
   FormatCard
.. code-block:: modula2
    PROCEDURE FormatCard (str: ARRAY OF CHAR): ConvResults;
      (* Returns the format of the string value for conversion to CARDINAL.
       *)
    
.. index::
   ValueCard
.. code-block:: modula2
    PROCEDURE ValueCard (str: ARRAY OF CHAR): CARDINAL;
      (* Returns the value corresponding to the unsigned whole number string
         value str if str is well-formed; otherwise raises the WholeConv
         exception.
       *)
    
.. index::
   LengthCard
.. code-block:: modula2
    PROCEDURE LengthCard (card: CARDINAL): CARDINAL;
      (* Returns the number of characters in the string representation of
         card.
       *)
    
.. index::
   IsWholeConvException
.. code-block:: modula2
    PROCEDURE IsWholeConvException (): BOOLEAN;
      (* Returns TRUE if the current coroutine is in the exceptional execution
         state because of the raising of an exception in a routine from this
         module; otherwise returns FALSE.
      *)
    
    END WholeConv.

@c @node gm2-libs-iso/WholeIO, gm2-libs-iso/WholeStr, gm2-libs-iso/WholeConv, M2 ISO Libraries
gm2-libs-iso/WholeIO
--------------------

.. code-block:: modula2
    DEFINITION MODULE WholeIO;

  (* Input and output of whole numbers in decimal text form
     over specified channels.  The read result is of the
     type IOConsts.ReadResults.
  *)
    
    IMPORT IOChan;
    
      (* The text form of a signed whole number is
           ["+" | "-"], decimal digit, {decimal digit}
    
         The text form of an unsigned whole number is
           decimal digit, {decimal digit}
      *)
    
.. index::
   ReadInt
.. code-block:: modula2
    PROCEDURE ReadInt (cid: IOChan.ChanId; VAR int: INTEGER);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of a signed whole number.  The
         value of this number is assigned to int.  The read result
         is set to the value allRight, outOfRange, wrongFormat,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteInt
.. code-block:: modula2
    PROCEDURE WriteInt (cid: IOChan.ChanId; int: INTEGER;
                        width: CARDINAL);
      (* Writes the value of int to cid in text form, in a field of
         the given minimum width. *)
    
.. index::
   ReadCard
.. code-block:: modula2
    PROCEDURE ReadCard (cid: IOChan.ChanId; VAR card: CARDINAL);
      (* Skips leading spaces, and removes any remaining characters
         from cid that form part of an unsigned whole number.  The
         value of this number is assigned to card. The read result
         is set to the value allRight, outOfRange, wrongFormat,
         endOfLine, or endOfInput.
      *)
    
.. index::
   WriteCard
.. code-block:: modula2
    PROCEDURE WriteCard (cid: IOChan.ChanId; card: CARDINAL;
                         width: CARDINAL);
      (* Writes the value of card to cid in text form, in a field
         of the given minimum width. *)
    
    END WholeIO.

@c @node gm2-libs-iso/WholeStr, gm2-libs-iso/wrapsock, gm2-libs-iso/WholeIO, M2 ISO Libraries
gm2-libs-iso/WholeStr
---------------------

.. code-block:: modula2
    DEFINITION MODULE WholeStr;

  (* Whole-number/string conversions *)
    
    IMPORT
      ConvTypes;
    
    TYPE
.. index::
   pair: ConvResults; (type)
.. code-block:: modula2
      ConvResults = ConvTypes.ConvResults;
      (* strAllRight, strOutOfRange, strWrongFormat, strEmpty *)
    
    (* the string form of a signed whole number is
         ["+" | "-"], decimal digit, {decimal digit}
    *)
    
.. index::
   StrToInt
.. code-block:: modula2
    PROCEDURE StrToInt (str: ARRAY OF CHAR; VAR int: INTEGER;
                        VAR res: ConvResults);
      (* Ignores any leading spaces in str. If the subsequent
         characters in str are in the format of a signed whole
         number, assigns a corresponding value to int. Assigns
         a value indicating the format of str to res.
      *)
    
.. index::
   IntToStr
.. code-block:: modula2
    PROCEDURE IntToStr (int: INTEGER; VAR str: ARRAY OF CHAR);
      (* Converts the value of int to string form and copies the
         possibly truncated result to str. *)
    
    (* the string form of an unsigned whole number is
         decimal digit, {decimal digit}
    *)
    
.. index::
   StrToCard
.. code-block:: modula2
    PROCEDURE StrToCard (str: ARRAY OF CHAR;
                         VAR card: CARDINAL;
                         VAR res: ConvResults);
      (* Ignores any leading spaces in str. If the subsequent
         characters in str are in the format of an unsigned
         whole number, assigns a corresponding value to card.
         Assigns a value indicating the format of str to res.
      *)
    
.. index::
   CardToStr
.. code-block:: modula2
    PROCEDURE CardToStr (card: CARDINAL; VAR str: ARRAY OF CHAR);
      (* Converts the value of card to string form and copies the
         possibly truncated result to str. *)
    
    END WholeStr.

@c @node gm2-libs-iso/wrapsock, gm2-libs-iso/wraptime, gm2-libs-iso/WholeStr, M2 ISO Libraries
gm2-libs-iso/wrapsock
---------------------

.. code-block:: modula2
    DEFINITION MODULE wrapsock ;

(*
    Description: provides a set of wrappers to some client side
                 tcp socket primatives.
*)
    
    FROM SYSTEM IMPORT ADDRESS ;
    FROM ChanConsts IMPORT OpenResults ;
    
    
    TYPE
.. index::
   pair: clientInfo; (type)
.. code-block:: modula2
       clientInfo = ADDRESS ;
    
    
    (*
       clientOpen - returns an ISO Modula-2 OpenResult.
                    It attempts to connect to:  hostname:portNo.
                    If successful then the data structure, c,
                    will have its fields initialized.
    *)
    
.. index::
   clientOpen
.. code-block:: modula2
    PROCEDURE clientOpen (c: clientInfo;
                          hostname: ADDRESS;
                          length: CARDINAL;
                          portNo: CARDINAL) : OpenResults ;
    
    
    (*
       clientOpenIP - returns an ISO Modula-2 OpenResult.
                      It attempts to connect to:  ipaddress:portNo.
                      If successful then the data structure, c,
                      will have its fields initialized.
    *)
    
.. index::
   clientOpenIP
.. code-block:: modula2
    PROCEDURE clientOpenIP (c: clientInfo;
                            ip: CARDINAL;
                            portNo: CARDINAL) : OpenResults ;
    
    
    (*
       getClientPortNo - returns the portNo from structure, c.
    *)
    
.. index::
   getClientPortNo
.. code-block:: modula2
    PROCEDURE getClientPortNo (c: clientInfo) : CARDINAL ;
    
    
    (*
       getClientHostname - fills in the hostname of the server
                           the to which the client is connecting.
    *)
    
.. index::
   getClientHostname
.. code-block:: modula2
    PROCEDURE getClientHostname (c: clientInfo;
                                 hostname: ADDRESS; high: CARDINAL) ;
    
    
    (*
       getClientSocketFd - returns the sockFd from structure, c.
    *)
    
.. index::
   getClientSocketFd
.. code-block:: modula2
    PROCEDURE getClientSocketFd (c: clientInfo) : INTEGER ;
    
    
    (*
       getClientIP - returns the sockFd from structure, s.
    *)
    
.. index::
   getClientIP
.. code-block:: modula2
    PROCEDURE getClientIP (c: clientInfo) : CARDINAL ;
    
    
    (*
       getPushBackChar - returns TRUE if a pushed back character
                         is available.
    *)
    
.. index::
   getPushBackChar
.. code-block:: modula2
    PROCEDURE getPushBackChar (c: clientInfo; VAR ch: CHAR) : BOOLEAN ;
    
    
    (*
       setPushBackChar - returns TRUE if it is able to push back a
                         character.
    *)
    
.. index::
   setPushBackChar
.. code-block:: modula2
    PROCEDURE setPushBackChar (c: clientInfo; ch: CHAR) : BOOLEAN ;
    
    
    (*
       getSizeOfClientInfo - returns the sizeof (opaque data type).
    *)
    
.. index::
   getSizeOfClientInfo
.. code-block:: modula2
    PROCEDURE getSizeOfClientInfo () : CARDINAL ;
    
    
    END wrapsock.

@c @node gm2-libs-iso/wraptime, , gm2-libs-iso/wrapsock, M2 ISO Libraries
gm2-libs-iso/wraptime
---------------------

.. code-block:: modula2
    DEFINITION MODULE wraptime ;

(*
    Description: provides an interface to various time related
                 entities on the underlying host operating system.
                 It provides access to the glibc/libc functions:
                 gettimeofday, settimeofday and localtime_r.
*)
    
    FROM SYSTEM IMPORT ADDRESS ;
    
    TYPE
.. index::
   pair: timeval; (type)
.. code-block:: modula2
       timeval  = ADDRESS ;
.. index::
   pair: timezone; (type)
.. code-block:: modula2
       timezone = ADDRESS ;
.. index::
   pair: tm; (type)
.. code-block:: modula2
       tm       = ADDRESS ;
    
    
    (*
       InitTimeval - returns a newly created opaque type.
    *)
    
.. index::
   InitTimeval
.. code-block:: modula2
    PROCEDURE InitTimeval () : timeval ;
    
    
    (*
       KillTimeval - deallocates the memory associated with an
                     opaque type.
    *)
    
.. index::
   KillTimeval
.. code-block:: modula2
    PROCEDURE KillTimeval (tv: timeval) : timeval ;
    
    
    (*
       InitTimezone - returns a newly created opaque type.
    *)
    
.. index::
   InitTimezone
.. code-block:: modula2
    PROCEDURE InitTimezone () : timezone ;
    
    
    (*
       KillTimezone - deallocates the memory associated with an
                      opaque type.
    *)
    
.. index::
   KillTimezone
.. code-block:: modula2
    PROCEDURE KillTimezone (tv: timezone) : timezone ;
    
    
    (*
       InitTM - returns a newly created opaque type.
    *)
    
.. index::
   InitTM
.. code-block:: modula2
    PROCEDURE InitTM () : tm ;
    
    
    (*
       KillTM - deallocates the memory associated with an
                opaque type.
    *)
    
.. index::
   KillTM
.. code-block:: modula2
    PROCEDURE KillTM (tv: tm) : tm ;
    
    
    (*
       gettimeofday - calls gettimeofday(2) with the same parameters, tv,
                      and, tz.  It returns 0 on success.
    *)
    
.. index::
   gettimeofday
.. code-block:: modula2
    PROCEDURE gettimeofday (tv: timeval; tz: timezone) : INTEGER ;
    
    
    (*
       settimeofday - calls settimeofday(2) with the same parameters, tv,
                      and, tz.  It returns 0 on success.
    *)
    
.. index::
   settimeofday
.. code-block:: modula2
    PROCEDURE settimeofday (tv: timeval; tz: timezone) : INTEGER ;
    
    
    (*
       GetFractions - returns the tv_usec field inside the timeval structure
                      as a CARDINAL.
    *)
    
.. index::
   GetFractions
.. code-block:: modula2
    PROCEDURE GetFractions (tv: timeval) : CARDINAL ;
    
    
    (*
       localtime_r - returns the tm parameter, m, after it has been assigned with
                     appropriate contents determined by, tv.  Notice that
                     this procedure function expects, timeval, as its first
                     parameter and not a time_t (as expected by the posix
                     equivalent).  This avoids having to expose a time_t
                     system dependant definition.
    *)
    
.. index::
   localtime_r
.. code-block:: modula2
    PROCEDURE localtime_r (tv: timeval; m: tm) : tm ;
    
    
    (*
       GetYear - returns the year from the structure, m.
    *)
    
.. index::
   GetYear
.. code-block:: modula2
    PROCEDURE GetYear (m: tm) : CARDINAL ;
    
    
    (*
       GetMonth - returns the month from the structure, m.
    *)
    
.. index::
   GetMonth
.. code-block:: modula2
    PROCEDURE GetMonth (m: tm) : CARDINAL ;
    
    
    (*
       GetDay - returns the day of the month from the structure, m.
    *)
    
.. index::
   GetDay
.. code-block:: modula2
    PROCEDURE GetDay (m: tm) : CARDINAL ;
    
    
    (*
       GetHour - returns the hour of the day from the structure, m.
    *)
    
.. index::
   GetHour
.. code-block:: modula2
    PROCEDURE GetHour (m: tm) : CARDINAL ;
    
    
    (*
       GetMinute - returns the minute within the hour from the structure, m.
    *)
    
.. index::
   GetMinute
.. code-block:: modula2
    PROCEDURE GetMinute (m: tm) : CARDINAL ;
    
    
    (*
       GetSecond - returns the seconds in the minute from the structure, m.
                   The return value will always be in the range 0..59.
                   A leap minute of value 60 will be truncated to 59.
    *)
    
.. index::
   GetSecond
.. code-block:: modula2
    PROCEDURE GetSecond (m: tm) : CARDINAL ;
    
    
    (*
       GetSummerTime - returns a boolean indicating whether summer time is
                       set.
    *)
    
.. index::
   GetSummerTime
.. code-block:: modula2
    PROCEDURE GetSummerTime (tz: timezone) : BOOLEAN ;
    
    
    (*
       GetDST - returns the number of minutes west of GMT.
    *)
    
.. index::
   GetDST
.. code-block:: modula2
    PROCEDURE GetDST (tz: timezone) : INTEGER ;
    
    
    (*
       SetTimeval - sets the fields in timeval, tv, with:
                    second, minute, hour, day, month, year, fractions.
    *)
    
.. index::
   SetTimeval
.. code-block:: modula2
    PROCEDURE SetTimeval (tv: timeval;
                          second, minute, hour, day,
                          month, year, yday, wday, isdst: CARDINAL) ;
    
    
    (*
       SetTimezone - set the timezone field inside timeval, tv.
    *)
    
.. index::
   SetTimezone
.. code-block:: modula2
    PROCEDURE SetTimezone (tv: timeval;
                           zone: CARDINAL; minuteswest: INTEGER) ;
    
    
    END wraptime.


@c ------------------------------------------------------------
