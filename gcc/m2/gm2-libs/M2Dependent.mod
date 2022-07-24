(* M2Dependent.mod implements the run time module dependencies.

Copyright (C) 2022 Free Software Foundation, Inc.
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


FROM libc IMPORT abort, exit, write, getenv, printf ;
(* FROM Builtins IMPORT strncmp, strcmp ;  not available during bootstrap.  *)
FROM M2LINK IMPORT ForcedModuleInitOrder, StaticInitialization, PtrToChar ;
FROM ASCII IMPORT nul, nl ;
FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;
FROM StrLib IMPORT StrCopy, StrLen, StrEqual ;

IMPORT M2RTS ;


TYPE
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
                               name      : ADDRESS ;
                               init,
                               fini      : ArgCVEnvP ;
                               dependency: DependencyList ;
                               prev,
                               next      : ModuleChain ;
                            END ;

VAR
   Modules        : ARRAY DependencyState OF ModuleChain ;
   Initialized,
   ModuleTrace,
   DependencyTrace,
   PreTrace,
   PostTrace,
   ForceTrace     : BOOLEAN ;


(*
   CreateModule - creates a new module entry and returns the
                  ModuleChain.
*)

PROCEDURE CreateModule (name: ADDRESS;
                        init, fini:  ArgCVEnvP;
                        dependencies: PROC) : ModuleChain ;
VAR
   mptr: ModuleChain ;
BEGIN
   NEW (mptr) ;
   mptr^.name := name ;
   mptr^.init := init ;
   mptr^.fini := fini ;
   mptr^.dependency.proc := dependencies ;
   mptr^.dependency.state := unregistered ;
   mptr^.prev := NIL ;
   mptr^.next := NIL ;
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
   LookupModuleN - lookup module from the state list.  The string is limited
                   to nchar.
*)

PROCEDURE LookupModuleN (state: DependencyState;
                         name: ADDRESS; nchar: CARDINAL) : ModuleChain ;
VAR
   ptr: ModuleChain ;
BEGIN
   IF Modules[state] # NIL
   THEN
      ptr := Modules[state] ;
      REPEAT
         IF strncmp (ptr^.name, name, nchar) = 0
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

PROCEDURE LookupModule (state: DependencyState; name: ADDRESS) : ModuleChain ;
BEGIN
   RETURN LookupModuleN (state, name, strlen (name))
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
      IF (str[i] = "\") AND (i < high)
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
   IF (a # NIL) AND (b # NIL) AND (n > 0)
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
BEGIN
   IF flag
   THEN
      toCString (str) ;
      printf (str, arg)
   END
END traceprintf2 ;


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

PROCEDURE ResolveDependant (mptr: ModuleChain; currentmodule: ADDRESS) ;
BEGIN
   IF mptr = NIL
   THEN
      traceprintf (DependencyTrace, "   module has not been registered via a global constructor\n");
   ELSE
      IF onChain (started, mptr)
      THEN
         traceprintf (DependencyTrace, "   processing...\n");
      ELSE
         moveTo (started, mptr) ;
         traceprintf2 (DependencyTrace, "   starting: %s\n",
                       currentmodule);
         mptr^.dependency.proc ;  (* Invoke and process the dependency graph.  *)
         traceprintf2 (DependencyTrace, "   finished: %s\n",
                       currentmodule);
         moveTo (ordered, mptr)
      END
   END
END ResolveDependant ;


(*
   RequestDependant - used to specify that modulename is dependant upon
                      module dependantmodule.  It only takes effect
                      if we are not using StaticInitialization.
*)

PROCEDURE RequestDependant (modulename, dependantmodule: ADDRESS) ;
BEGIN
   CheckInitialized ;
   IF NOT StaticInitialization
   THEN
      PerformRequestDependant (modulename, dependantmodule)
   END
END RequestDependant ;


(*
   PerformRequestDependant - the current modulename has a dependancy upon
                             dependantmodule.  If dependantmodule is NIL then
                             modulename has no further dependants and it can be
                             resolved.
*)

PROCEDURE PerformRequestDependant (modulename, dependantmodule: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   traceprintf2 (DependencyTrace, "  module %s", modulename) ;
   IF dependantmodule = NIL
   THEN
      traceprintf2 (DependencyTrace, " has finished its import graph\n", modulename) ;
      mptr := LookupModule (unordered, modulename) ;
      IF mptr # NIL
      THEN
         traceprintf2 (DependencyTrace, "  module %s is now ordered\n", modulename) ;
         moveTo (ordered, mptr)
      END
   ELSE
      traceprintf2 (DependencyTrace, " imports from %s\n", dependantmodule) ;
      mptr := LookupModule (ordered, dependantmodule) ;
      IF mptr = NIL
      THEN
         traceprintf2 (DependencyTrace, "  module %s is not ordered\n", dependantmodule) ;
         mptr := LookupModule (unordered, dependantmodule) ;
         IF mptr = NIL
         THEN
            traceprintf2 (DependencyTrace, "  module %s is not unordered\n", dependantmodule) ;
            mptr := LookupModule (started, dependantmodule) ;
            IF mptr = NIL
            THEN
               traceprintf2 (DependencyTrace, "  module %s has not started\n", dependantmodule) ;
               traceprintf2 (DependencyTrace, "  module %s attempting to import from",
                             modulename) ;
               traceprintf2 (DependencyTrace, " %s which has not registered itself via a constructor\n",
                             dependantmodule)
            ELSE
               traceprintf2 (DependencyTrace, "  module %s has registered itself and has started\n", dependantmodule)
            END
         ELSE
            traceprintf2 (DependencyTrace, "  module %s resolving\n", dependantmodule) ;
            ResolveDependant (mptr, dependantmodule)
         END
      ELSE
         traceprintf2 (DependencyTrace, "  module %s ", modulename) ;
         traceprintf2 (DependencyTrace, " dependant %s is ordered\n", dependantmodule)
      END
   END
END PerformRequestDependant ;


(*
   ResolveDependencies - resolve dependencies for currentmodule.
*)

PROCEDURE ResolveDependencies (currentmodule: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   mptr := LookupModule (unordered, currentmodule) ;
   WHILE mptr # NIL DO
      traceprintf2 (DependencyTrace, "   attempting to resolve the dependants for %s\n",
                    currentmodule);
      ResolveDependant (mptr, currentmodule) ;
      mptr := Modules[unordered]
   END
END ResolveDependencies ;


(*
   DisplayModuleInfo - displays all module in the state.
*)

PROCEDURE DisplayModuleInfo (state: DependencyState; name: ARRAY OF CHAR) ;
VAR
   mptr: ModuleChain ;
BEGIN
   IF Modules[state] # NIL
   THEN
      printf ("%s modules\n", ADR (name)) ;
      mptr := Modules[state] ;
      REPEAT
         printf ("  %s", mptr^.name) ;
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
VAR
   mptr: ModuleChain ;
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
   ForceDependencies - if the user has specified a forced order then we override
                       the dynamic ordering with the preference.
*)

PROCEDURE ForceDependencies ;
VAR
   mptr,
   userChain: ModuleChain ;
   count    : CARDINAL ;
   pc, start: PtrToChar ;
BEGIN
   IF ForcedModuleInitOrder # NIL
   THEN
      userChain := NIL ;
      pc := ForcedModuleInitOrder ;
      start := pc ;
      count := 0 ;
      WHILE pc^ # nul DO
         IF pc^ = ','
         THEN
            mptr := LookupModuleN (ordered, start, count) ;
            IF mptr # NIL
            THEN
               mptr^.dependency.forced := TRUE ;
               moveTo (user, mptr)
            END ;
            INC (pc) ;
            start := pc ;
            count := 0
         ELSE
            INC (pc) ;
            INC (count)
         END
      END ;
      IF start # pc
      THEN
         mptr := LookupModuleN (ordered, start, count) ;
         IF mptr # NIL
         THEN
            mptr^.dependency.forced := TRUE ;
            moveTo (user, mptr)
         END
      END ;
      combine (user, ordered)
   END
END ForceDependencies ;


(*
   ConstructModules - resolve dependencies and then call each
                      module constructor in turn.
*)

PROCEDURE ConstructModules (applicationmodule: ADDRESS;
                            argc: INTEGER; argv, envp: ADDRESS) ;
VAR
   mptr: ModuleChain ;
   nulp: ArgCVEnvP ;
BEGIN
   CheckInitialized ;
   traceprintf2 (ModuleTrace, "application module: %s\n", applicationmodule);
   mptr := LookupModule (unordered, applicationmodule) ;
   IF mptr # NIL
   THEN
      mptr^.dependency.appl := TRUE
   END ;
   traceprintf (PreTrace, "Pre resolving dependents\n");
   DumpModuleData (PreTrace) ;
   ResolveDependencies (applicationmodule) ;
   traceprintf (PreTrace, "Post resolving dependents\n");
   DumpModuleData (PostTrace) ;
   ForceDependencies ;
   traceprintf (ForceTrace, "After user forcing ordering\n");
   DumpModuleData (ForceTrace) ;
   IF Modules[ordered] = NIL
   THEN
      traceprintf2 (ModuleTrace, "  module: %s has not registered itself using a global constructor\n", applicationmodule);
      traceprintf2 (ModuleTrace, "  hint try compile and linking using: gm2 %s.mod\n", applicationmodule);
      traceprintf2 (ModuleTrace, "  or try using: gm2 -fscaffold-static %s.mod\n",
      applicationmodule);
   ELSE
      mptr := Modules[ordered] ;
      REPEAT
         IF mptr^.dependency.forc
         THEN
            traceprintf2 (ModuleTrace, "initializing module: %s for C\n", mptr^.name);
         ELSE
            traceprintf2 (ModuleTrace, "initializing module: %s\n", mptr^.name);
         END ;
         IF mptr^.dependency.appl
         THEN
            traceprintf2 (ModuleTrace, "application module: %s\n", mptr^.name);
            traceprintf (ModuleTrace, "  calling M2RTS_ExecuteInitialProcedures\n");
            M2RTS.ExecuteInitialProcedures ;
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

PROCEDURE DeconstructModules (applicationmodule: ADDRESS;
                              argc: INTEGER; argv, envp: ADDRESS) ;
VAR
   mptr: ModuleChain ;
BEGIN
   traceprintf2 (ModuleTrace, "application module finishing: %s\n", applicationmodule);
   IF Modules[ordered] = NIL
   THEN
      traceprintf (ModuleTrace, "  no ordered modules found during finishing\n")
   ELSE
      traceprintf (ModuleTrace, "ExecuteTerminationProcedures\n") ;
      M2RTS.ExecuteTerminationProcedures ;
      traceprintf (ModuleTrace, "terminating modules in sequence\n") ;
      mptr := Modules[ordered]^.prev ;
      REPEAT
         IF mptr^.dependency.forc
         THEN
            traceprintf2 (ModuleTrace, "finalizing module: %s for C\n", mptr^.name);
         ELSE
            traceprintf2 (ModuleTrace, "finalizing module: %s\n", mptr^.name);
         END ;
         mptr^.fini (argc, argv, envp) ;
         mptr := mptr^.prev
      UNTIL mptr = Modules[ordered]^.prev
   END
END DeconstructModules ;


(*
   RegisterModule - adds module name to the list of outstanding
                    modules which need to have their dependencies
                    explored to determine initialization order.
*)

PROCEDURE RegisterModule (name: ADDRESS;
                          init, fini:  ArgCVEnvP;
                          dependencies: PROC) ;
BEGIN
   CheckInitialized ;
   IF NOT StaticInitialization
   THEN
      traceprintf2 (ModuleTrace, "module: %s registering\n",
                    name);
      moveTo (unordered,
              CreateModule (name, init, fini, dependencies))
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
                     "all,module,pre,post,dep,force".  all turns them all on.
                     The flag meanings are as follows and flags the are in
                     execution order.

                     module   generate trace info as the modules are registered.
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
   pc := getenv (ADR ("GCC_M2LINK_RTFLAG")) ;
   WHILE (pc # NIL) AND (pc^ # nul) DO
      IF equal (pc, "all")
      THEN
         ModuleTrace := TRUE ;
         DependencyTrace := TRUE ;
         PreTrace := TRUE ;
         PostTrace := TRUE ;
         ForceTrace := TRUE ;
         INC (pc, 3)
      ELSIF equal (pc, "module")
      THEN
         ModuleTrace := TRUE ;
         INC (pc, 6)
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
   SetupDebugFlags ;
   FOR state := MIN (DependencyState) TO MAX (DependencyState) DO
      Modules[state] := NIL
   END
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


BEGIN
   CheckInitialized
END M2Dependent.
