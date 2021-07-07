(* keyc maintains the C name scope and avoids C/C++ name conflicts.
   Copyright (C) 2016-2021 Free Software Foundation, Inc.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

IMPLEMENTATION MODULE keyc ;

FROM mcPretty IMPORT pretty, print, prints, setNeedSpace, noSpace ;
FROM Storage IMPORT ALLOCATE ;
FROM DynamicStrings IMPORT InitString, KillString, ConCat, ConCatChar,
                           Mark, string, InitStringCharStar ;
FROM symbolKey IMPORT symbolTree, getSymKey, putSymKey, initTree, killTree ;
FROM nameKey IMPORT makeKey, makekey, keyToCharStar ;
FROM mcOptions IMPORT getHPrefix, getGccConfigSystem ;


TYPE
   scope = POINTER TO RECORD
                         scoped : node ;
			 symbols: symbolTree ;
			 next   : scope ;
                      END ;

VAR
   stack,
   freeList  : scope ;
   keywords,
   macros    : symbolTree ;

   initializedCP,
   initializedGCC,

   seenIntMin,
   seenUIntMin,
   seenLongMin,
   seenULongMin,
   seenCharMin,
   seenUCharMin,
   seenIntMax,
   seenUIntMax,
   seenLongMax,
   seenULongMax,
   seenCharMax,
   seenUCharMax,
   seenLabs,
   seenAbs,
   seenFabs,
   seenFabsl,
   seenSize_t,
   seenSSize_t,

   seenUnistd,
   seenSysTypes,
   seenThrow,
   seenFree,
   seenMalloc,
   seenStorage,
   seenProc,
   seenTrue,
   seenFalse,
   seenNull,
   seenMemcpy,
   seenException,
   seenComplex,
   seenM2RTS,
   seenStrlen,
   seenCtype    : BOOLEAN ;


(*
   checkGccConfigSystem - issues the GCC include config.h, include system.h
                          instead of the standard host include.
*)

PROCEDURE checkGccConfigSystem (p: pretty) ;
BEGIN
   IF getGccConfigSystem ()
   THEN
      IF NOT initializedGCC
      THEN
         initializedGCC := TRUE ;
         print (p, '#include "config.h"\n');
         print (p, '#include "system.h"\n');
      END
   END
END checkGccConfigSystem ;


(*
   useStorage - indicate we have used storage.
*)

PROCEDURE useStorage ;
BEGIN
   seenStorage := TRUE
END useStorage ;


(*
   useFree - indicate we have used free.
*)

PROCEDURE useFree ;
BEGIN
   seenFree := TRUE
END useFree ;


(*
   useMalloc - indicate we have used malloc.
*)

PROCEDURE useMalloc ;
BEGIN
   seenMalloc := TRUE
END useMalloc ;


(*
   useProc - indicate we have used proc.
*)

PROCEDURE useProc ;
BEGIN
   seenProc := TRUE
END useProc ;


(*
   useTrue - indicate we have used TRUE.
*)

PROCEDURE useTrue ;
BEGIN
   seenTrue := TRUE
END useTrue ;


(*
   useFalse - indicate we have used FALSE.
*)

PROCEDURE useFalse ;
BEGIN
   seenFalse := TRUE
END useFalse ;


(*
   useNull - indicate we have used NULL.
*)

PROCEDURE useNull ;
BEGIN
   seenNull := TRUE
END useNull ;


(*
   useMemcpy - indicate we have used memcpy.
*)

PROCEDURE useMemcpy ;
BEGIN
   seenMemcpy := TRUE
END useMemcpy ;


(*
   useIntMin - indicate we have used INT_MIN.
*)

PROCEDURE useIntMin ;
BEGIN
   seenIntMin := TRUE
END useIntMin ;


(*
   useUIntMin - indicate we have used UINT_MIN.
*)

PROCEDURE useUIntMin ;
BEGIN
   seenUIntMin := TRUE
END useUIntMin ;


(*
   useLongMin - indicate we have used LONG_MIN.
*)

PROCEDURE useLongMin ;
BEGIN
   seenLongMin := TRUE
END useLongMin ;


(*
   useULongMin - indicate we have used ULONG_MIN.
*)

PROCEDURE useULongMin ;
BEGIN
   seenULongMin := TRUE
END useULongMin ;


(*
   useCharMin - indicate we have used CHAR_MIN.
*)

PROCEDURE useCharMin ;
BEGIN
   seenCharMin := TRUE
END useCharMin ;


(*
   useUCharMin - indicate we have used UCHAR_MIN.
*)

PROCEDURE useUCharMin ;
BEGIN
   seenUCharMin := TRUE
END useUCharMin ;


(*
   useUIntMin - indicate we have used UINT_MIN.
*)

PROCEDURE useUIntMin ;
BEGIN
   seenUIntMin := TRUE
END useUIntMin ;


(*
   useIntMax - indicate we have used INT_MAX.
*)

PROCEDURE useIntMax ;
BEGIN
   seenIntMax := TRUE
END useIntMax ;


(*
   useUIntMax - indicate we have used UINT_MAX.
*)

PROCEDURE useUIntMax ;
BEGIN
   seenUIntMax := TRUE
END useUIntMax ;


(*
   useLongMax - indicate we have used LONG_MAX.
*)

PROCEDURE useLongMax ;
BEGIN
   seenLongMax := TRUE
END useLongMax ;


(*
   useULongMax - indicate we have used ULONG_MAX.
*)

PROCEDURE useULongMax ;
BEGIN
   seenULongMax := TRUE
END useULongMax ;


(*
   useCharMax - indicate we have used CHAR_MAX.
*)

PROCEDURE useCharMax ;
BEGIN
   seenCharMax := TRUE
END useCharMax ;


(*
   useUCharMax - indicate we have used UChar_MAX.
*)

PROCEDURE useUCharMax ;
BEGIN
   seenUCharMax := TRUE
END useUCharMax ;


(*
   useUIntMax - indicate we have used UINT_MAX.
*)

PROCEDURE useUIntMax ;
BEGIN
   seenUIntMax := TRUE
END useUIntMax ;


(*
   useSize_t - indicate we have used size_t.
*)

PROCEDURE useSize_t ;
BEGIN
   seenSize_t := TRUE
END useSize_t ;


(*
   useSSize_t - indicate we have used ssize_t.
*)

PROCEDURE useSSize_t ;
BEGIN
   seenSSize_t := TRUE ;
   seenSysTypes := TRUE
END useSSize_t ;


(*
   useLabs - indicate we have used labs.
*)

PROCEDURE useLabs ;
BEGIN
   seenLabs := TRUE
END useLabs ;


(*
   useAbs - indicate we have used abs.
*)

PROCEDURE useAbs ;
BEGIN
   seenAbs := TRUE
END useAbs ;


(*
   useFabs - indicate we have used fabs.
*)

PROCEDURE useFabs ;
BEGIN
   seenFabs := TRUE
END useFabs ;


(*
   useFabsl - indicate we have used fabsl.
*)

PROCEDURE useFabsl ;
BEGIN
   seenFabsl := TRUE
END useFabsl ;


(*
   useM2RTS - indicate we have used M2RTS in the converted code.
*)

PROCEDURE useM2RTS ;
BEGIN
   seenM2RTS := TRUE
END useM2RTS ;


(*
   useStrlen - indicate we have used strlen in the converted code.
*)

PROCEDURE useStrlen ;
BEGIN
   seenStrlen := TRUE
END useStrlen ;


(*
   useCtype - indicate we have used the toupper function.
*)

PROCEDURE useCtype ;
BEGIN
   seenCtype := TRUE
END useCtype ;


(*
   checkCtype -
*)

PROCEDURE checkCtype (p: pretty) ;
BEGIN
   IF seenCtype
   THEN
      checkGccConfigSystem (p);
      IF getGccConfigSystem ()
      THEN
         (* GCC header files use a safe variant.  *)
         print (p, "#include <safe-ctype.h>\n")
      ELSE
         print (p, "#include <ctype.h>\n")
      END
   END
END checkCtype ;


(*
   checkAbs - check to see if the abs family, size_t or ssize_t have been used.
*)

PROCEDURE checkAbs (p: pretty) ;
BEGIN
   IF seenLabs OR seenAbs OR seenFabs OR seenFabsl OR seenSize_t OR seenSSize_t
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, "#include <stdlib.h>\n")
      END
   END
END checkAbs ;


(*
   checkLimits -
*)

PROCEDURE checkLimits (p: pretty) ;
BEGIN
   IF seenMemcpy OR seenIntMin OR seenUIntMin OR
      seenLongMin OR seenULongMin OR seenCharMin OR
      seenUCharMin OR seenUIntMin OR seenIntMax OR
      seenUIntMax OR seenLongMax OR seenULongMax OR
      seenCharMax OR seenUCharMax OR seenUIntMax
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, "#include <limits.h>\n")
      END
   END
END checkLimits ;


(*
   checkFreeMalloc -
*)

PROCEDURE checkFreeMalloc (p: pretty) ;
BEGIN
   IF seenFree OR seenMalloc
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, "#include <stdlib.h>\n")
      END
   END
END checkFreeMalloc ;


(*
   checkStorage -
*)

PROCEDURE checkStorage (p: pretty) ;
BEGIN
   IF seenStorage
   THEN
      print (p, '#   include "') ;
      prints (p, getHPrefix ()) ;
      print (p, 'Storage.h"\n')
   END
END checkStorage ;


(*
   checkProc -
*)

PROCEDURE checkProc (p: pretty) ;
BEGIN
   IF seenProc
   THEN
      print (p, "#   if !defined (PROC_D)\n") ;
      print (p, "#      define PROC_D\n") ;
      print (p, "       typedef void (*PROC_t) (void);\n") ;
      print (p, "       typedef struct { PROC_t proc; } PROC;\n") ;
      print (p, "#   endif\n\n")
   END
END checkProc ;


(*
   checkTrue -
*)

PROCEDURE checkTrue (p: pretty) ;
BEGIN
   IF seenTrue
   THEN
      print (p, "#   if !defined (TRUE)\n") ;
      print (p, "#      define TRUE (1==1)\n") ;
      print (p, "#   endif\n\n")
   END
END checkTrue ;


(*
   checkFalse -
*)

PROCEDURE checkFalse (p: pretty) ;
BEGIN
   IF seenFalse
   THEN
      print (p, "#   if !defined (FALSE)\n") ;
      print (p, "#      define FALSE (1==0)\n") ;
      print (p, "#   endif\n\n")
   END
END checkFalse ;


(*
   checkNull -
*)

PROCEDURE checkNull (p: pretty) ;
BEGIN
   IF seenNull
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, "#include <stddef.h>\n")
      END
   END
END checkNull ;


(*
   checkMemcpy -
*)

PROCEDURE checkMemcpy (p: pretty) ;
BEGIN
   IF seenMemcpy OR seenStrlen
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, "#include <string.h>\n")
      END
   END
END checkMemcpy ;


(*
   checkM2RTS -
*)

PROCEDURE checkM2RTS (p: pretty) ;
BEGIN
   IF seenM2RTS
   THEN
      print (p, '#   include "') ;
      prints (p, getHPrefix ()) ;
      print (p, 'M2RTS.h"\n')
   END
END checkM2RTS ;


(*
   useException - use the exceptions module, mcrts.
*)

PROCEDURE useException ;
BEGIN
   seenException := TRUE
END useException ;


(*
   checkException - check to see if exceptions were used.
*)

PROCEDURE checkException (p: pretty) ;
BEGIN
   IF seenException
   THEN
      print (p, '#   include "Gmcrts.h"\n')
   END
END checkException ;


(*
   useThrow - use the throw function.
*)

PROCEDURE useThrow ;
BEGIN
   seenThrow := TRUE
END useThrow ;


(*
   checkThrow - check to see if the throw function is used.
*)

PROCEDURE checkThrow (p: pretty) ;
BEGIN
   IF seenThrow
   THEN
      print (p, '#   include "sys/cdefs.h"\n') ;
      (* print (p, 'extern void throw (int);\n')  *)
   END
END checkThrow ;


(*
   useUnistd - need to use unistd.h call using open/close/read/write require this header.
*)

PROCEDURE useUnistd ;
BEGIN
   seenUnistd := TRUE
END useUnistd ;


(*
   checkUnistd - check to see if the unistd.h header file is required.
*)

PROCEDURE checkUnistd (p: pretty) ;
BEGIN
   IF seenUnistd
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, '#include <unistd.h>\n')
      END
   END
END checkUnistd ;


(*
   useComplex - use the complex data type.
*)

PROCEDURE useComplex ;
BEGIN
   seenComplex := TRUE
END useComplex ;


(*
   checkComplex - check to see if the type complex was used.
*)

PROCEDURE checkComplex (p: pretty) ;
BEGIN
   IF seenComplex
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, '#   include <complex.h>\n')
      END
   END
END checkComplex ;


(*
   checkSysTypes - emit header for sys/types.h if necessary.
*)

PROCEDURE checkSysTypes (p: pretty) ;
BEGIN
   IF seenSysTypes
   THEN
      checkGccConfigSystem (p);
      IF NOT getGccConfigSystem ()
      THEN
         print (p, '#   include <sys/types.h>\n')
      END
   END
END checkSysTypes ;


(*
   fixNullPointerConst - fixup for NULL on some C++11 systems.
*)

PROCEDURE fixNullPointerConst (p: pretty) ;
BEGIN
   IF seenNull
   THEN
      print (p, '#if defined(__cplusplus)\n') ;
      print (p, '#   undef NULL\n') ;      
      print (p, '#   define NULL 0\n') ;
      print (p, '#endif\n')
   END
END fixNullPointerConst ;


(*
   genDefs - generate definitions or includes for all
             macros and prototypes used.
*)

PROCEDURE genDefs (p: pretty) ;
BEGIN
   checkGccConfigSystem (p) ;
   checkFreeMalloc (p) ;
   checkProc (p) ;
   checkTrue (p) ;
   checkFalse (p) ;
   checkNull (p) ;
   checkMemcpy (p) ;
   checkLimits (p) ;
   checkAbs (p) ;
   checkStorage (p) ;
   checkException (p) ;
   checkComplex (p) ;
   checkCtype (p) ;
   checkUnistd (p) ;
   checkSysTypes (p) ;
   checkM2RTS (p) ;
   checkThrow (p) ;
   fixNullPointerConst (p)
END genDefs ;


(*
   new -
*)

PROCEDURE new (n: node) : scope ;
VAR
   s: scope ;
BEGIN
   IF freeList = NIL
   THEN
      NEW (s)
   ELSE
      s := freeList ;
      freeList := freeList^.next
   END ;
   RETURN s
END new ;


(*
   enterScope - enter a scope defined by, n.
*)

PROCEDURE enterScope (n: node) ;
VAR
   s: scope ;
BEGIN
   s := new (n) ;
   WITH s^ DO
      scoped := n ;
      symbols := initTree () ;
      next := stack
   END ;
   stack := s
END enterScope ;


(*
   leaveScope - leave the scope defined by, n.
*)

PROCEDURE leaveScope (n: node) ;
VAR
   s: scope ;
BEGIN
   IF n = stack^.scoped
   THEN
      s := stack ;
      stack := stack^.next ;
      WITH s^ DO
         scoped := NIL ;
         killTree (symbols) ;
         next := NIL
      END
   ELSE
      HALT
   END
END leaveScope ;


(*
   mangle1 - returns TRUE if name is unique if we add _
             to its end.
*)

PROCEDURE mangle1 (n: Name; VAR m: String; scopes: BOOLEAN) : BOOLEAN ;
BEGIN
   m := KillString (m) ;
   m := InitStringCharStar (keyToCharStar (n)) ;
   m := ConCatChar (m, '_') ;
   RETURN NOT clash (makekey (string (m)), scopes)
END mangle1 ;


(*
   mangle2 - returns TRUE if name is unique if we prepend _
             to, n.
*)

PROCEDURE mangle2 (n: Name; VAR m: String; scopes: BOOLEAN) : BOOLEAN ;
BEGIN
   m := KillString (m) ;
   m := InitStringCharStar (keyToCharStar (n)) ;
   m := ConCat (InitString ('_'), Mark (m)) ;
   RETURN NOT clash (makekey (string (m)), scopes)
END mangle2 ;


(*
   mangleN - keep adding '_' to the end of n until it
             no longer clashes.
*)

PROCEDURE mangleN (n: Name; VAR m: String; scopes: BOOLEAN) : BOOLEAN ;
BEGIN
   m := KillString (m) ;
   m := InitStringCharStar (keyToCharStar (n)) ;
   LOOP
      m := ConCatChar (m, '_') ;
      IF NOT clash (makekey (string (m)), scopes)
      THEN
         RETURN TRUE
      END
   END
END mangleN ;


(*
   clash - returns TRUE if there is a clash with name, n,
           in the current scope or C keywords or C macros.
*)

PROCEDURE clash (n: Name; scopes: BOOLEAN) : BOOLEAN ;
BEGIN
   IF (getSymKey (macros, n) # NIL) OR
      (getSymKey (keywords, n) # NIL)
   THEN
      RETURN TRUE
   END ;
   RETURN scopes AND (getSymKey (stack^.symbols, n) # NIL)
END clash ;


(*
   cname - attempts to declare a symbol with name, n, in the
           current scope.  If there is no conflict with the
           target language then NIL is returned, otherwise
           a mangled name is returned as a String.
           If scopes is FALSE then only the keywords and
           macros are detected for a clash (all scoping
           is ignored).
*)

PROCEDURE cname (n: Name; scopes: BOOLEAN) : String ;
VAR
   m: String ;
BEGIN
   m := NIL ;
   IF clash (n, scopes)
   THEN
      IF mangle1 (n, m, scopes) OR mangle2 (n, m, scopes) OR mangleN (n, m, scopes)
      THEN
         IF scopes
         THEN
            (* no longer a clash with, m, so add it to the current scope.  *)
            n := makekey (string (m)) ;
            putSymKey (stack^.symbols, n, m)
         END
      ELSE
         (* mangleN must always succeed.  *)
         HALT
      END
   ELSIF scopes
   THEN
      (* no clash, add it to the current scope.  *)
      putSymKey (stack^.symbols, n, InitStringCharStar (keyToCharStar (n)))
   END ;
   RETURN m
END cname ;


(*
   cnamen - attempts to declare a symbol with name, n, in the
            current scope.  If there is no conflict with the
            target language then NIL is returned, otherwise
            a mangled name is returned as a Name
            If scopes is FALSE then only the keywords and
            macros are detected for a clash (all scoping
            is ignored).
*)

PROCEDURE cnamen (n: Name; scopes: BOOLEAN) : Name ;
VAR
   m: String ;
BEGIN
   m := NIL ;
   IF clash (n, scopes)
   THEN
      IF mangle1 (n, m, scopes) OR mangle2 (n, m, scopes) OR mangleN (n, m, scopes)
      THEN
         n := makekey (string (m)) ;
         IF scopes
         THEN
            (* no longer a clash with, m, so add it to the current scope.  *)
            putSymKey (stack^.symbols, n, m)
         END
      ELSE
         (* mangleN must always succeed.  *)
         HALT
      END
   ELSIF scopes
   THEN
      (* no clash, add it to the current scope.  *)
      putSymKey (stack^.symbols, n, InitStringCharStar (keyToCharStar (n)))
   END ;
   m := KillString (m) ;
   RETURN n
END cnamen ;


(*
   cp - include C++ keywords and standard declarations to avoid.
*)

PROCEDURE cp ;
BEGIN
   IF NOT initializedCP
   THEN
      initializedCP := TRUE ;
      initCP
   END
END cp ;


(*
   initCP - add the extra keywords and standard definitions used by C++.
*)

PROCEDURE initCP ;
BEGIN
   add (keywords, 'delete') ;
   add (keywords, 'try') ;
   add (keywords, 'catch') ;
   add (keywords, 'operator') ;
   add (keywords, 'complex')
END initCP ;


(*
   add -
*)

PROCEDURE add (s: symbolTree; a: ARRAY OF CHAR) ;
BEGIN
   putSymKey (s, makeKey (a), InitString (a))
END add ;


(*
   initMacros - macros and library function names to avoid.
*)

PROCEDURE initMacros ;
BEGIN
   macros := initTree () ;
   add (macros, 'FILE') ;
   add (macros, 'EOF') ;
   add (macros, 'stdio') ;
   add (macros, 'stdout') ;
   add (macros, 'stderr') ;
   add (macros, 'write') ;
   add (macros, 'read') ;
   add (macros, 'exit') ;
   add (macros, 'abs') ;
   add (macros, 'optarg') ;
   add (macros, 'div') ;
   add (macros, 'sin') ;
   add (macros, 'cos') ;
   add (macros, 'tan') ;
   add (macros, 'log10') ;
   add (macros, 'I') ;
   add (macros, 'csqrt') ;
   add (macros, 'strlen') ;
   add (macros, 'strcpy') ;
   add (macros, 'free') ;
   add (macros, 'malloc') ;
   add (macros, 'time') ;
   add (macros, 'main') ;
   add (macros, 'true') ;
   add (macros, 'false')
END initMacros ;


(*
   initKeywords - keywords to avoid.
*)

PROCEDURE initKeywords ;
BEGIN
   keywords := initTree () ;
   add (keywords, 'auto') ;
   add (keywords, 'break') ;
   add (keywords, 'case') ;
   add (keywords, 'char') ;
   add (keywords, 'const') ;
   add (keywords, 'continue') ;
   add (keywords, 'default') ;
   add (keywords, 'do') ;
   add (keywords, 'double') ;
   add (keywords, 'else') ;
   add (keywords, 'enum') ;
   add (keywords, 'extern') ;
   add (keywords, 'float') ;
   add (keywords, 'for') ;
   add (keywords, 'goto') ;
   add (keywords, 'if') ;
   add (keywords, 'int') ;
   add (keywords, 'long') ;
   add (keywords, 'register') ;
   add (keywords, 'return') ;
   add (keywords, 'short') ;
   add (keywords, 'signed') ;
   add (keywords, 'sizeof') ;
   add (keywords, 'static') ;
   add (keywords, 'struct') ;
   add (keywords, 'switch') ;
   add (keywords, 'typedef') ;
   add (keywords, 'union') ;
   add (keywords, 'unsigned') ;
   add (keywords, 'void') ;
   add (keywords, 'volatile') ;
   add (keywords, 'while') ;
   add (keywords, 'and') ;
   add (keywords, 'or') ;
   add (keywords, 'not') ;
   add (keywords, 'throw') ;
   add (keywords, 'new')
END initKeywords ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   seenUnistd := FALSE ;
   seenThrow := FALSE ;
   seenFree := FALSE ;
   seenMalloc := FALSE ;
   seenStorage := FALSE ;
   seenProc := FALSE ;
   seenTrue := FALSE ;
   seenFalse := FALSE ;
   seenNull := FALSE ;
   seenMemcpy := FALSE ;
   seenIntMin := FALSE ;
   seenUIntMin := FALSE ;
   seenLongMin := FALSE ;
   seenULongMin := FALSE ;
   seenCharMin := FALSE ;
   seenUCharMin := FALSE ;
   seenUIntMin := FALSE ;
   seenIntMax := FALSE ;
   seenUIntMax := FALSE ;
   seenLongMax := FALSE ;
   seenULongMax := FALSE ;
   seenCharMax := FALSE ;
   seenUCharMax := FALSE ;
   seenUIntMax := FALSE ;
   seenLabs := FALSE ;
   seenAbs := FALSE ;
   seenFabs := FALSE ;
   seenFabsl := FALSE ;
   seenException := FALSE ;
   seenComplex := FALSE ;
   seenM2RTS := FALSE ;
   seenStrlen := FALSE ;
   seenCtype := FALSE ;
   seenSize_t := FALSE ;
   seenSSize_t := FALSE ;
   seenSysTypes := FALSE ;
   initializedCP := FALSE ;
   initializedGCC := FALSE ;

   stack := NIL ;
   freeList := NIL ;
   initKeywords ;
   initMacros
END init ;


BEGIN
   init
END keyc.
