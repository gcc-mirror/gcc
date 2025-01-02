(* M2Dependent.mod implements the run time module dependencies.

Copyright (C) 2022-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Dependent ;


FROM libc IMPORT abort, exit, write, getenv, printf, snprintf, strncpy ;
FROM ASCII IMPORT nul, nl ;
FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;
FROM StrLib IMPORT StrCopy, StrLen, StrEqual ;


TYPE
   PtrToChar = POINTER TO CHAR ;

   DependencyState = (unregistered, unordered, started, ordered, user) ;

   DependencyList = RECORD
                       proc  : PROC ;
                       (* Has this module order been forced by the user?  *)
                       forced,
                       (* Is the module a definition module for C?  *)
                       forc  : BOOLEAN ;
                       appl  : BOOLEAN ;  (* The application module?  *)
                       state : DependencyState ;
                    END ;

   ModuleChain = POINTER TO RECORD
                               name,
                               libname   : ADDRESS ;
                               init,
                               fini      : ArgCVEnvP ;
                               dependency: DependencyList ;
                               prev,
                               next      : ModuleChain ;
                            END ;

   ProcedureList = RECORD
                      head, tail: ProcedureChain
                   END ;

   ProcedureChain = POINTER TO RECORD
                                  p   : PROC ;
                                  prev,
                                  next: ProcedureChain ;
                                END ;

VAR
   Modules              : ARRAY DependencyState OF ModuleChain ;
   DynamicInitialization,
   Initialized,
   WarningTrace,
   ModuleTrace,
   HexTrace,
   DependencyTrace,
   PreTrace,
   PostTrace,
   ForceTrace           : BOOLEAN ;
   InitialProc,
   TerminateProc        : ProcedureList ;


(*
   InitDependencyList - initialize all fields of DependencyList.
*)

PROCEDURE InitDependencyList (VAR depList: DependencyList;
                              proc: PROC; state: DependencyState) ;
BEGIN
   depList.proc := proc ;
   depList.forced := FALSE ;
   depList.forc := FALSE ;
   depList.appl := FALSE ;
   depList.state := state
END InitDependencyList ;


(*
   CreateModule - creates a new module entry and returns the
                  ModuleChain.
*)

PROCEDURE CreateModule (name, libname: ADDRESS;
                        init, fini:  ArgCVEnvP;
                        dependencies: PROC) : ModuleChain ;
VAR
   mptr: ModuleChain ;
BEGIN
   NEW (mptr) ;
   mptr^.name := name ;
   mptr^.libname := libname ;
   mptr^.init := init ;
   mptr^.fini := fini ;
   InitDependencyList (mptr^.dependency, dependencies, unregistered) ;
   mptr^.prev := NIL ;
   mptr^.next := NIL ;
   IF HexTrace
   THEN
      printf ("   (init: %p  fini: %p", init, fini) ;
      printf ("  dep: %p)", dependencies)
   END ;
   RETURN mptr
END CreateModule ;


(*
   AppendModule - append chain to end of the list.
*)

PROCEDURE AppendModule (VAR head: ModuleChain; chain: ModuleChain) ;
BEGIN
   IF head = NIL
   THEN
      head := chain ;
      chain^.prev := chain ;
      chain^.next := chain
   ELSE
      chain^.next := head ;   (* Add Item to the end of list.  *)
      chain^.prev  := head^.prev ;
      head^.prev^.next := chain ;
      head^.prev := chain
   END
END AppendModule ;


(*
   RemoveModule - remove chain from double linked list head.
*)

PROCEDURE RemoveModule (VAR head: ModuleChain; chain: ModuleChain) ;
BEGIN
   IF (chain^.next=head) AND (chain=head)
   THEN
      head := NIL
   ELSE
      IF head=chain
      THEN
         head := head^.next
      END ;
      chain^.prev^.next := chain^.next ;
      chain^.next^.prev := chain^.prev
   END
END RemoveModule ;


(*
   onChain - returns TRUE if mptr is on the Modules[state] list.
*)

PROCEDURE onChain (state: DependencyState; mptr: ModuleChain) : BOOLEAN ;
VAR
   ptr: ModuleChain ;
BEGIN
   IF Modules[state] # NIL
   THEN
      ptr := Modules[state] ;
      REPEAT
         IF ptr = mptr
         THEN
            RETURN TRUE
         END ;
         ptr := ptr^.next
      UNTIL ptr=Modules[state]
   END ;
   RETURN FALSE
END onChain ;


(*
   max -
*)

PROCEDURE max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a > b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END max ;


(*
   min -
*)

PROCEDURE min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a < b
   THEN
      RETURN a
   ELSE
      RETURN b
   END
END min ;


(*
   LookupModuleN - lookup module from the state list.
                   The strings lengths are known.
*)

PROCEDURE LookupModuleN (state: DependencyState;
                         name: ADDRESS; namelen: CARDINAL;
                         libname: ADDRESS; libnamelen: CARDINAL) : ModuleChain ;
VAR
   ptr: ModuleChain ;
BEGIN
   IF Modules[state] # NIL
   THEN
      ptr := Modules[state] ;
      REPEAT
         IF (strncmp (ptr^.name, name,
                      max (namelen, strlen (ptr^.name))) = 0) AND
            (strncmp (ptr^.libname, libname,
                      max (libnamelen, strlen (ptr^.libname))) = 0)
         THEN
            RETURN ptr
         END ;
         ptr := ptr^.next
      UNTIL ptr = Modules[state]
   END ;
   RETURN NIL
END LookupModuleN ;


(*
   LookupModule - lookup and return the ModuleChain pointer containing
                  module name from a particular list.
*)

PROCEDURE LookupModule (state: DependencyState; name, libname: ADDRESS) : ModuleChain ;
BEGIN
   RETURN LookupModuleN (state,
                         name, strlen (name),
                         libname, strlen (libname))
END LookupModule ;


(*
   toCString - replace any character sequence \n into a newline.
*)

PROCEDURE toCString (VAR str: ARRAY OF CHAR) ;
VAR
   high, i, j: CARDINAL ;
BEGIN
   i := 0 ;
   high := HIGH (str) ;
   WHILE i < high DO
      IF (i < high) AND (str[i] = "\")
      THEN
         IF str[i+1] = "n"
         THEN
            str[i] := nl ;
            j := i+1 ;
            WHILE j < high DO
               str[j] := str[j+1] ;
               INC (j)
            END
         END
      END ;
      INC (i)
   END
END toCString ;


(*
   strcmp - return 0 if both strings are equal.
            We cannot use Builtins.def during bootstrap.
*)

PROCEDURE strcmp (a, b: PtrToChar) : INTEGER ;
BEGIN
   IF (a # NIL) AND (b # NIL)
   THEN
      IF a = b
      THEN
         RETURN 0
      ELSE
         WHILE a^ = b^ DO
            IF a^ = nul
            THEN
               RETURN 0
            END ;
            INC (a) ;
            INC (b)
         END
      END
   END ;
   RETURN 1
END strcmp ;


(*
   strncmp - return 0 if both strings are equal.
             We cannot use Builtins.def during bootstrap.
*)

PROCEDURE strncmp (a, b: PtrToChar; n: CARDINAL) : INTEGER ;
BEGIN
   IF n = 0
   THEN
      RETURN 0
   ELSIF (a # NIL) AND (b # NIL)
   THEN
      IF a = b
      THEN
         RETURN 0
      ELSE
         WHILE (a^ = b^) AND (n > 0) DO
            IF (a^ = nul) OR (n = 1)
            THEN
               RETURN 0
            END ;
            INC (a) ;
            INC (b) ;
            DEC (n)
         END
      END
   END ;
   RETURN 1
END strncmp ;


(*
   strlen - returns the length of string.
*)

PROCEDURE strlen (string: PtrToChar) : INTEGER ;
VAR
   count: INTEGER ;
BEGIN
   IF string = NIL
   THEN
      RETURN 0
   ELSE
      count := 0 ;
      WHILE string^ # nul DO
         INC (string) ;
         INC (count)
      END ;
      RETURN count
   END
END strlen ;


(*
   traceprintf - wrap printf with a boolean flag.
*)

PROCEDURE traceprintf (flag: BOOLEAN; str: ARRAY OF CHAR) ;
BEGIN
   IF flag
   THEN
      toCString (str) ;
      printf (str)
   END
END traceprintf ;


(*
   traceprintf2 - wrap printf with a boolean flag.
*)

PROCEDURE traceprintf2 (flag: BOOLEAN; str: ARRAY OF CHAR; arg: ADDRESS) ;
VAR
   ch: CHAR ;
BEGIN
   IF flag
   THEN
      toCString (str) ;
      IF arg = NIL
      THEN
         ch := 0C ;
         arg := ADR (ch)
      END ;
      printf (str, arg)
   END
END traceprintf2 ;


(*
   traceprintf3 - wrap printf with a boolean flag.
*)

PROCEDURE traceprintf3 (flag: BOOLEAN; str: ARRAY OF CHAR;
                        arg1, arg2: ADDRESS) ;
VAR
   ch: CHAR ;
BEGIN
   IF flag
   THEN
      toCString (str) ;
      IF arg1 = NIL
      THEN
         ch := 0C ;
         arg1 := ADR (ch)
      END ;
      IF arg2 = NIL
      THEN
         ch := 0C ;
         arg2 := ADR (ch)
      END ;
      printf (str, arg1, arg2)
   END
END traceprintf3 ;


(*
   moveTo - moves mptr to the new list determined by newstate.
            It updates the mptr state appropriately.
*)

PROCEDURE moveTo (newstate: DependencyState; mptr: ModuleChain) ;
BEGIN
   IF onChain (mptr^.dependency.state, mptr)
   THEN
      RemoveModule (Modules[mptr^.dependency.state], mptr)
   END ;
   mptr^.dependency.state := newstate ;
   AppendModule (Modules[mptr^.dependency.state], mptr)
END moveTo ;


(*
   ResolveDependant -
*)

PROCEDURE ResolveDependant (mptr: ModuleChain; currentmodule, libname: ADDRESS) ;
BEGIN
   IF mptr = NIL
   THEN
      traceprintf3 (DependencyTrace,
                    "   module %s [%s] has not been registered via a global constructor\n",
                    currentmodule, libname);
   ELSE
      IF onChain (started, mptr)
      THEN
         traceprintf (DependencyTrace, "   processing...\n");
      ELSE
         moveTo (started, mptr) ;
         traceprintf3 (DependencyTrace, "   starting: %s [%s]\n",
                       currentmodule, libname);
         mptr^.dependency.proc ;  (* Invoke and process the dependency graph.  *)
         traceprintf3 (DependencyTrace, "   finished: %s [%s]\n",
                       currentmodule, libname);
         moveTo (ordered, mptr)
      END
   END
END ResolveDependant ;


(*
   RequestDependant - used to specify that modulename is dependant upon
                      module dependantmodule.  It only takes effect
                      if we are using DynamicInitialization.
*)

PROCEDURE RequestDependant (modulename, libname,
                            dependantmodule, dependantlibname: ADDRESS) ;
BEGIN
   CheckInitialized ;
   PerformRequestDependant (modulename, libname,
                            dependantmodule, dependantlibname)
END RequestDependant ;


(*
   PerformRequestDependant - the current modulename has a dependancy upon
                             dependantmodule.  If dependantmodule is NIL then
                             modulename has no further dependants and it can be
                             resolved.
*)

PROCEDURE PerformRequestDependant (modulename, libname,
                                   dependantmodule, dependantlibname: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   traceprintf3 (DependencyTrace, "  module %s [%s]", modulename, libname) ;
   IF dependantmodule = NIL
   THEN
      traceprintf (DependencyTrace, " has finished its import graph\n") ;
      mptr := LookupModule (unordered, modulename, libname) ;
      IF mptr # NIL
      THEN
         traceprintf3 (DependencyTrace, "  module %s [%s] is now ordered\n",
                       modulename, libname) ;
         moveTo (ordered, mptr)
      END
   ELSE
      traceprintf3 (DependencyTrace, " imports from %s [%s]\n",
                    dependantmodule, dependantlibname) ;
      mptr := LookupModule (ordered, dependantmodule, dependantlibname) ;
      IF mptr = NIL
      THEN
         traceprintf3 (DependencyTrace, "  module %s [%s] is not ordered\n",
                       dependantmodule, dependantlibname) ;
         mptr := LookupModule (unordered, dependantmodule, dependantlibname) ;
         IF mptr = NIL
         THEN
            traceprintf3 (DependencyTrace, "  module %s [%s] is not unordered\n",
                          dependantmodule, dependantlibname) ;
            mptr := LookupModule (started, dependantmodule, dependantlibname) ;
            IF mptr = NIL
            THEN
               traceprintf3 (DependencyTrace, "  module %s [%s] has not started\n",
                             dependantmodule, dependantlibname) ;
               traceprintf3 (DependencyTrace, "  module %s [%s] attempting to import from",
                             modulename, libname) ;
               traceprintf3 (DependencyTrace, " %s [%s] which has not registered itself via a constructor\n",
                             dependantmodule, dependantlibname)
            ELSE
               traceprintf3 (DependencyTrace, "  module %s [%s] has registered itself and has started\n",
                             dependantmodule, dependantlibname)
            END
         ELSE
            traceprintf3 (DependencyTrace, "  module %s [%s] resolving\n", dependantmodule, dependantlibname) ;
            ResolveDependant (mptr, dependantmodule, dependantlibname)
         END
      ELSE
         traceprintf3 (DependencyTrace, "  module %s [%s]", modulename, libname) ;
         traceprintf3 (DependencyTrace, " dependant %s [%s] is ordered\n", dependantmodule, dependantlibname)
      END
   END
END PerformRequestDependant ;


(*
   ResolveDependencies - resolve dependencies for currentmodule, libname.
*)

PROCEDURE ResolveDependencies (currentmodule, libname: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   mptr := LookupModule (unordered, currentmodule, libname) ;
   WHILE mptr # NIL DO
      traceprintf3 (DependencyTrace, "   attempting to resolve the dependants for %s [%s]\n",
                    currentmodule, libname);
      ResolveDependant (mptr, currentmodule, libname) ;
      mptr := Modules[unordered]
   END
END ResolveDependencies ;


(*
   DisplayModuleInfo - displays all module in the state.
*)

PROCEDURE DisplayModuleInfo (state: DependencyState; desc: ARRAY OF CHAR) ;
VAR
   mptr : ModuleChain ;
   count: CARDINAL ;
BEGIN
   IF Modules[state] # NIL
   THEN
      printf ("%s modules\n", ADR (desc)) ;
      mptr := Modules[state] ;
      count := 0 ;
      REPEAT
         IF mptr^.name = NIL
         THEN
            printf ("  %d  %s []", count, mptr^.name)
         ELSE
            printf ("  %d  %s [%s]", count, mptr^.name, mptr^.libname)
         END ;
         INC (count) ;
         IF mptr^.dependency.appl
         THEN
            printf (" application")
         END ;
         IF mptr^.dependency.forc
         THEN
            printf (" for C")
         END ;
         IF mptr^.dependency.forced
         THEN
            printf (" forced ordering")
         END ;
         printf ("\n") ;
         mptr := mptr^.next ;
      UNTIL mptr = Modules[state]
   END
END DisplayModuleInfo ;


(*
   DumpModuleData -
*)

PROCEDURE DumpModuleData (flag: BOOLEAN) ;
BEGIN
   IF flag
   THEN
      DisplayModuleInfo (unregistered, "unregistered") ;
      DisplayModuleInfo (unordered, "unordered") ;
      DisplayModuleInfo (started, "started") ;
      DisplayModuleInfo (ordered, "ordered") ;
   END
END DumpModuleData ;


(*
   combine - dest := src + dest.  Places src at the front of list dest.
             Pre condition:  src, dest are lists.
             Post condition : dest := src + dest
                              src := NIL.
*)

PROCEDURE combine (src, dest: DependencyState) ;
VAR
   last: ModuleChain ;
BEGIN
   WHILE Modules[src] # NIL DO
      last := Modules[src]^.prev ;
      moveTo (ordered, last) ;
      Modules[dest] := last  (* New item is at the head.  *)
   END
END combine ;


(*
   tracemodule -
*)

PROCEDURE tracemodule (flag: BOOLEAN; modname: ADDRESS; modlen: CARDINAL; libname: ADDRESS; liblen: CARDINAL) ;
VAR
   buffer: ARRAY [0..100] OF CHAR ;
   len   : CARDINAL ;
BEGIN
   IF flag
   THEN
      len := min (modlen, SIZE (buffer)-1) ;
      strncpy (ADR(buffer), modname, len) ;
      buffer[len] := 0C ;
      printf ("%s ", ADR (buffer)) ;
      len := min (liblen, SIZE (buffer)-1) ;
      strncpy (ADR(buffer), libname, len) ;
      buffer[len] := 0C ;
      printf (" [%s]", ADR (buffer))
   END
END tracemodule ;


(*
   ForceModule -
*)

PROCEDURE ForceModule (modname: ADDRESS; modlen: CARDINAL;
                       libname: ADDRESS; liblen: CARDINAL) ;
VAR
   mptr: ModuleChain ;
BEGIN
   traceprintf (ForceTrace, "forcing module: ") ;
   tracemodule (ForceTrace, modname, modlen, libname, liblen) ;
   traceprintf (ForceTrace, "\n") ;
   mptr := LookupModuleN (ordered, modname, modlen, libname, liblen) ;
   IF mptr # NIL
   THEN
      mptr^.dependency.forced := TRUE ;
      moveTo (user, mptr)
   END
END ForceModule ;


(*
   ForceDependencies - if the user has specified a forced order then we override
                       the dynamic ordering with the preference.
*)

PROCEDURE ForceDependencies (overrideliborder: ADDRESS) ;
VAR
   len,
   modlen,
   liblen   : CARDINAL ;
   modname,
   libname,
   pc, start: PtrToChar ;
BEGIN
   IF overrideliborder # NIL
   THEN
      traceprintf2 (ForceTrace, "user forcing order: %s\n", overrideliborder) ;
      pc := overrideliborder ;
      start := pc ;
      len := 0 ;
      modname := NIL ;
      modlen := 0 ;
      libname := NIL ;
      liblen := 0 ;
      WHILE pc^ # nul DO
         CASE pc^ OF

         ':':  libname := start ;
               liblen := len ;
               len := 0 ;
               INC (pc) ;
               start := pc |
         ',':  modname := start ;
               modlen := len ;
               ForceModule (modname, modlen, libname, liblen) ;
               libname := NIL ;
               liblen := 0 ;
               modlen := 0 ;
               len := 0 ;
               INC (pc) ;
               start := pc
         ELSE
            INC (pc) ;
            INC (len)
         END
      END ;
      IF start # pc
      THEN
         ForceModule (start, len, libname, liblen)
      END ;
      combine (user, ordered)
   END
END ForceDependencies ;


(*
   CheckApplication - check to see that the application is the last entry in the list.
                      This might happen if the application only imports FOR C modules.
*)

PROCEDURE CheckApplication ;
VAR
   mptr,
   appl: ModuleChain ;
BEGIN
   mptr := Modules[ordered] ;
   IF mptr # NIL
   THEN
      appl := NIL ;
      REPEAT
         IF mptr^.dependency.appl
         THEN
            appl := mptr
         ELSE
            mptr := mptr^.next
         END
      UNTIL (appl # NIL) OR (mptr=Modules[ordered]) ;
      IF appl # NIL
      THEN
         RemoveModule (Modules[ordered], appl) ;
         AppendModule (Modules[ordered], appl)
      END
   END
END CheckApplication ;


(*
   ConstructModules - resolve dependencies and then call each
                      module constructor in turn.
*)

PROCEDURE ConstructModules (applicationmodule, libname,
                            overrideliborder: ADDRESS;
                            argc: INTEGER; argv, envp: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   CheckInitialized ;
   DynamicInitialization := TRUE ;  (* This procedure is only called if we desire dynamic initialization.  *)
   traceprintf3 (ModuleTrace, "application module: %s [%s]\n",
                 applicationmodule, libname);
   mptr := LookupModule (unordered, applicationmodule, libname) ;
   IF mptr # NIL
   THEN
      mptr^.dependency.appl := TRUE
   END ;
   traceprintf (PreTrace, "Pre resolving dependents\n");
   DumpModuleData (PreTrace) ;
   ResolveDependencies (applicationmodule, libname) ;
   traceprintf (PreTrace, "Post resolving dependents\n");
   DumpModuleData (PostTrace) ;
   ForceDependencies (overrideliborder) ;
   traceprintf (ForceTrace, "After user forcing ordering\n");
   DumpModuleData (ForceTrace) ;
   CheckApplication ;
   traceprintf (ForceTrace, "After runtime forces application to the end\n");
   DumpModuleData (ForceTrace) ;
   IF Modules[ordered] = NIL
   THEN
      traceprintf3 (ModuleTrace, "  module: %s [%s] has not registered itself using a global constructor\n",
                    applicationmodule, libname);
      traceprintf2 (ModuleTrace, "  hint try compile and linking using: gm2 %s.mod\n", applicationmodule);
      traceprintf2 (ModuleTrace, "  or try using: gm2 -fscaffold-static %s.mod\n",
      applicationmodule);
   ELSE
      mptr := Modules[ordered] ;
      REPEAT
         IF mptr^.dependency.forc
         THEN
            traceprintf3 (ModuleTrace, "initializing module: %s [%s] for C\n", mptr^.name, mptr^.libname)
         ELSE
            traceprintf3 (ModuleTrace, "initializing module: %s [%s]\n", mptr^.name, mptr^.libname);
         END ;
         IF mptr^.dependency.appl
         THEN
            traceprintf3 (ModuleTrace, "application module: %s [%s]\n", mptr^.name, mptr^.libname);
            traceprintf (ModuleTrace, "  calling ExecuteInitialProcedures\n");
            ExecuteInitialProcedures ;
            traceprintf (ModuleTrace, "  calling application module\n");
         END ;
         mptr^.init (argc, argv, envp) ;
         mptr := mptr^.next
      UNTIL mptr = Modules[ordered]
   END
END ConstructModules ;


(*
   DeconstructModules - resolve dependencies and then call each
                        module constructor in turn.
*)

PROCEDURE DeconstructModules (applicationmodule, libname: ADDRESS;
                              argc: INTEGER; argv, envp: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   traceprintf3 (ModuleTrace, "application module finishing: %s [%s]\n",
                 applicationmodule, libname);
   IF Modules[ordered] = NIL
   THEN
      traceprintf (ModuleTrace, "  no ordered modules found during finishing\n")
   ELSE
      traceprintf (ModuleTrace, "ExecuteTerminationProcedures\n") ;
      ExecuteTerminationProcedures ;
      traceprintf (ModuleTrace, "terminating modules in sequence\n") ;
      mptr := Modules[ordered]^.prev ;
      REPEAT
         IF mptr^.dependency.forc
         THEN
            traceprintf3 (ModuleTrace, "finalizing module: %s [%s] for C\n",
                          mptr^.name, mptr^.libname)
         ELSE
            traceprintf3 (ModuleTrace, "finalizing module: %s [%s]\n",
                          mptr^.name, mptr^.libname)
         END ;
         mptr^.fini (argc, argv, envp) ;
         mptr := mptr^.prev
      UNTIL mptr = Modules[ordered]^.prev
   END
END DeconstructModules ;


(*
   warning3 - write format arg1 arg2 to stderr.
*)

PROCEDURE warning3 (format: ARRAY OF CHAR; arg1, arg2: ADDRESS) ;
VAR
   buffer: ARRAY [0..4096] OF CHAR ;
   len   : INTEGER ;
BEGIN
   IF WarningTrace
   THEN
      len := snprintf (ADR (buffer), SIZE (buffer), "warning: ") ;
      write (2, ADR (buffer), len) ;
      len := snprintf (ADR (buffer), SIZE (buffer), format, arg1, arg2) ;
      write (2, ADR (buffer), len)
   END
END warning3 ;


(*
   RegisterModule - adds module name to the list of outstanding
                    modules which need to have their dependencies
                    explored to determine initialization order.
*)

PROCEDURE RegisterModule (modulename, libname: ADDRESS;
                          init, fini:  ArgCVEnvP;
                          dependencies: PROC) ;
VAR
   mptr: ModuleChain ;
BEGIN
   CheckInitialized ;
   mptr := LookupModule (unordered, modulename, libname) ;
   IF mptr = NIL
   THEN
      traceprintf3 (ModuleTrace, "module: %s [%s] registering",
                    modulename, libname);
      moveTo (unordered,
              CreateModule (modulename, libname, init, fini, dependencies)) ;
      traceprintf (ModuleTrace, "\n") ;
   ELSE
      warning3 ("module: %s [%s] (ignoring duplicate registration)\n",
                modulename, libname)
   END
END RegisterModule ;


(*
   equal - return TRUE if C string cstr is equal to str.
*)

PROCEDURE equal (cstr: ADDRESS; str: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   RETURN strncmp (cstr, ADR (str), StrLen (str)) = 0
END equal ;


(*
   SetupDebugFlags - By default assigns ModuleTrace, DependencyTrace,
                     DumpPostInit to FALSE.  It checks the environment
                     GCC_M2LINK_RTFLAG which can contain
                     "all,module,hex,pre,post,dep,force".  all turns them all on.
                     The flag meanings are as follows and flags the are in
                     execution order.

                     module   generate trace info as the modules are registered.
                     hex      dump the modules ctor functions address in hex.
                     pre      generate a list of all modules seen prior to having
                              their dependancies resolved.
                     dep      display a trace as the modules are resolved.
                     post     generate a list of all modules seen after having
                              their dependancies resolved dynamically.
                     force    generate a list of all modules seen after having
                              their dependancies resolved and forced.
*)

PROCEDURE SetupDebugFlags ;
VAR
   pc: POINTER TO CHAR ;
BEGIN
   ModuleTrace := FALSE ;
   DependencyTrace := FALSE ;
   PostTrace := FALSE ;
   PreTrace := FALSE ;
   ForceTrace := FALSE ;
   HexTrace := FALSE ;
   WarningTrace := FALSE ;
   pc := getenv (ADR ("GCC_M2LINK_RTFLAG")) ;
   WHILE (pc # NIL) AND (pc^ # nul) DO
      IF equal (pc, "all")
      THEN
         ModuleTrace := TRUE ;
         DependencyTrace := TRUE ;
         PreTrace := TRUE ;
         PostTrace := TRUE ;
         ForceTrace := TRUE ;
         HexTrace := TRUE ;
         WarningTrace := TRUE ;
         INC (pc, 3)
      ELSIF equal (pc, "module")
      THEN
         ModuleTrace := TRUE ;
         INC (pc, 6)
      ELSIF equal (pc, "warning")
      THEN
         WarningTrace := TRUE ;
         INC (pc, 7)
      ELSIF equal (pc, "hex")
      THEN
         HexTrace := TRUE ;
         INC (pc, 3)
      ELSIF equal (pc, "dep")
      THEN
         DependencyTrace := TRUE ;
         INC (pc, 3)
      ELSIF equal (pc, "pre")
      THEN
         PreTrace := TRUE ;
         INC (pc, 3)
      ELSIF equal (pc, "post")
      THEN
         PostTrace := TRUE ;
         INC (pc, 4)
      ELSIF equal (pc, "force")
      THEN
         ForceTrace := TRUE ;
         INC (pc, 5)
      ELSE
         INC (pc)
      END
   END
END SetupDebugFlags ;


(*
   Init - initialize the debug flags and set all lists to NIL.
*)

PROCEDURE Init ;
VAR
   state: DependencyState ;
BEGIN
   InitProcList (InitialProc) ;
   InitProcList (TerminateProc) ;
   SetupDebugFlags ;
   FOR state := MIN (DependencyState) TO MAX (DependencyState) DO
      Modules[state] := NIL
   END ;
   DynamicInitialization := FALSE
END Init ;


(*
   CheckInitialized - checks to see if this module has been initialized
                      and if it has not it calls Init.  We need this
                      approach as this module is called by module ctors
                      before we reach main.
*)

PROCEDURE CheckInitialized ;
BEGIN
   IF NOT Initialized
   THEN
      Initialized := TRUE ;
      Init
   END
END CheckInitialized ;


(*
   ExecuteReverse - execute the procedure associated with procptr
                    and then proceed to try and execute all previous
                    procedures in the chain.
*)

PROCEDURE ExecuteReverse (procptr: ProcedureChain) ;
BEGIN
   WHILE procptr # NIL DO
      procptr^.p ;  (* Invoke the procedure.  *)
      procptr := procptr^.prev
   END
END ExecuteReverse ;


(*
   ExecuteTerminationProcedures - calls each installed termination procedure
                                  in reverse order.
*)

PROCEDURE ExecuteTerminationProcedures ;
BEGIN
   ExecuteReverse (TerminateProc.tail)
END ExecuteTerminationProcedures ;


(*
   ExecuteInitialProcedures - executes the initial procedures installed by
                              InstallInitialProcedure.
*)

PROCEDURE ExecuteInitialProcedures ;
BEGIN
   ExecuteReverse (InitialProc.tail)
END ExecuteInitialProcedures ;


(*
   AppendProc - append proc to the end of the procedure list
                defined by proclist.
*)

PROCEDURE AppendProc (VAR proclist: ProcedureList; proc: PROC) : BOOLEAN ;
VAR
   pdes: ProcedureChain ;
BEGIN
   NEW (pdes) ;
   WITH pdes^ DO
      p := proc ;
      prev := proclist.tail ;
      next := NIL
   END ;
   IF proclist.head = NIL
   THEN
      proclist.head := pdes
   END ;
   proclist.tail := pdes ;
   RETURN TRUE
END AppendProc ;


(*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE if the
                                 procedure is installed.
*)

PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;
BEGIN
   RETURN AppendProc (TerminateProc, p)
END InstallTerminationProcedure ;


(*
   InstallInitialProcedure - installs a procedure to be executed just
                             before the BEGIN code section of the
                             main program module.
*)

PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;
BEGIN
   RETURN AppendProc (InitialProc, p)
END InstallInitialProcedure ;


(*
   InitProcList - initialize the head and tail pointers to NIL.
*)

PROCEDURE InitProcList (VAR p: ProcedureList) ;
BEGIN
   p.head := NIL ;
   p.tail := NIL
END InitProcList ;


BEGIN
   CheckInitialized
END M2Dependent.
