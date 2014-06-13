------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                D E B U G                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Debug is

   ---------------------------------
   -- Summary of Debug Flag Usage --
   ---------------------------------

   --  Debug flags for compiler (GNAT1)

   --  da   Generate messages tracking semantic analyzer progress
   --  db   Show encoding of type names for debug output
   --  dc   List names of units as they are compiled
   --  dd   Dynamic allocation of tables messages generated
   --  de   List the entity table
   --  df   Full tree/source print (includes withed units)
   --  dg   Print source from tree (generated code only)
   --  dh   Generate listing showing loading of name table hash chains
   --  di   Generate messages for visibility linking/delinking
   --  dj   Suppress "junk null check" for access parameter values
   --  dk   Generate GNATBUG message on abort, even if previous errors
   --  dl   Generate unit load trace messages
   --  dm   Allow VMS features even if not OpenVMS version
   --  dn   Generate messages for node/list allocation
   --  do   Print source from tree (original code only)
   --  dp   Generate messages for parser scope stack push/pops
   --  dq   No auto-alignment of small records
   --  dr   Generate parser resynchronization messages
   --  ds   Print source from tree (including original and generated stuff)
   --  dt   Print full tree
   --  du   Uncheck categorization pragmas
   --  dv   Output trace of overload resolution
   --  dw   Print trace of semantic scope stack
   --  dx   Force expansion on, even if no code being generated
   --  dy   Print tree of package Standard
   --  dz   Print source of package Standard

   --  dA   All entities included in representation information output
   --  dB   Output debug encoding of type names and variants
   --  dC   Output debugging information on check suppression
   --  dD   Delete elaboration checks in inner level routines
   --  dE   Apply elaboration checks to predefined units
   --  dF   Front end data layout enabled
   --  dG   Generate all warnings including those normally suppressed
   --  dH   Hold (kill) call to gigi
   --  dI   Inhibit internal name numbering in gnatG listing
   --  dJ   Output debugging trace info for JGNAT (Java VM version of GNAT)
   --  dK   Kill all error messages
   --  dL   Output trace information on elaboration checking
   --  dM   Assume all variables are modified (no current values)
   --  dN   No file name information in exception messages
   --  dO   Output immediate error messages
   --  dP   Do not check for controlled objects in preelaborable packages
   --  dQ   Do not generate runtime check for duplicated external tag
   --  dR   Bypass check for correct version of s-rpc
   --  dS   Never convert numbers to machine numbers in Sem_Eval
   --  dT   Convert to machine numbers only for constant declarations
   --  dU   Enable garbage collection of unreachable entities
   --  dV   Enable viewing of all symbols in debugger
   --  dW   Disable warnings on calls for IN OUT parameters
   --  dX   Display messages on reads of potentially uninitialized scalars
   --  dY   Enable configurable run-time mode
   --  dZ   Generate listing showing the contents of the dispatch tables

   --  d.a  Force Target_Strict_Alignment mode to True
   --  d.b  Dump backend types
   --  d.c  Generate inline concatenation, do not call procedure
   --  d.d  Disable atomic synchronization
   --  d.e  Enable atomic synchronization
   --  d.f  Inhibit folding of static expressions
   --  d.g  Enable conversion of raise into goto
   --  d.h
   --  d.i  Ignore Warnings pragmas
   --  d.j  Generate listing of frontend inlined calls
   --  d.k  Enable new support for frontend inlining
   --  d.l  Use Ada 95 semantics for limited function returns
   --  d.m  For -gnatl, print full source only for main unit
   --  d.n  Print source file names
   --  d.o  Generate .NET listing of CIL code
   --  d.p  Enable the .NET CIL verifier
   --  d.q
   --  d.r  Enable OK_To_Reorder_Components in non-variant records
   --  d.s  Disable expansion of slice move, use memmove
   --  d.t  Disable static allocation of library level dispatch tables
   --  d.u  Enable Modify_Tree_For_C (update tree for c)
   --  d.v  Enable OK_To_Reorder_Components in variant records
   --  d.w  Do not check for infinite loops
   --  d.x  No exception handlers
   --  d.y
   --  d.z  Temporary ASIS kludge for why non-static messages

   --  d.A  Read/write Aspect_Specifications hash table to tree
   --  d.B
   --  d.C  Generate concatenation call, do not generate inline code
   --  d.D
   --  d.E  Turn selected errors into warnings
   --  d.F  Debug mode for GNATprove
   --  d.G  Ignore calls through generic formal parameters for elaboration
   --  d.H
   --  d.I  Do not ignore enum representation clauses in CodePeer mode
   --  d.J  Disable parallel SCIL generation mode
   --  d.K
   --  d.L  Depend on back end for limited types in if and case expressions
   --  d.M  Relaxed RM semantics
   --  d.N  Add node to all entities
   --  d.O  Dump internal SCO tables
   --  d.P  Previous (non-optimized) handling of length comparisons
   --  d.Q
   --  d.R  Restrictions in ali files in positional form
   --  d.S  Force Optimize_Alignment (Space)
   --  d.T  Force Optimize_Alignment (Time)
   --  d.U  Ignore indirect calls for static elaboration
   --  d.V
   --  d.W  Print out debugging information for Walk_Library_Items
   --  d.X
   --  d.Y
   --  d.Z

   --  d1   Error msgs have node numbers where possible
   --  d2   Eliminate error flags in verbose form error messages
   --  d3   Dump bad node in Comperr on an abort
   --  d4   Inhibit automatic krunch of predefined library unit files
   --  d5   Debug output for tree read/write
   --  d6   Default access unconstrained to thin pointers
   --  d7   Do not output version & file time stamp in -gnatv or -gnatl mode
   --  d8   Force opposite endianness in packed stuff
   --  d9   Allow lock free implementation

   --  d.1
   --  d.2
   --  d.3
   --  d.4
   --  d.5
   --  d.6
   --  d.7
   --  d.8
   --  d.9

   --  Debug flags for binder (GNATBIND)

   --  da  All links (including internal units) listed if there is a cycle
   --  db  Output information from Better_Choice
   --  dc  List units as they are chosen
   --  dd
   --  de  Elaboration dependencies including system units
   --  df
   --  dg
   --  dh
   --  di  Ignore_Errors mode for reading ali files
   --  dj
   --  dk
   --  dl
   --  dm
   --  dn  List details of manipulation of Num_Pred values
   --  do  Use old preference for elaboration order
   --  dp
   --  dq
   --  dr
   --  ds
   --  dt
   --  du  List units as they are acquired
   --  dv
   --  dw
   --  dx  Force binder to read xref information from ali files
   --  dy
   --  dz

   --  Debug flags used in package Make and its clients (e.g. GNATMAKE)

   --  da
   --  db
   --  dc
   --  dd
   --  de
   --  df  Only output file names, not path names, in log
   --  dg
   --  dh  Generate listing showing loading of name table hash chains
   --  di
   --  dj
   --  dk
   --  dl
   --  dm  Display the number of maximum simultaneous compilations
   --  dn  Do not delete temp files created by gnatmake
   --  do
   --  dp  Prints the contents of the Q used by Make.Compile_Sources
   --  dq  Prints source files as they are enqueued and dequeued
   --  dr
   --  ds
   --  dt  Display time stamps when there is a mismatch
   --  du  List units as their ali files are acquired
   --  dv
   --  dw  Prints the list of units withed by the unit currently explored
   --  dx
   --  dy
   --  dz

   --------------------------------------------
   -- Documentation for Compiler Debug Flags --
   --------------------------------------------

   --  da   Generate messages tracking semantic analyzer progress. A message
   --       is output showing each node as it gets analyzed, expanded,
   --       resolved, or evaluated. This option is useful for finding out
   --       exactly where a bomb during semantic analysis is occurring.

   --  db   In Exp_Dbug, certain type names are encoded to include debugging
   --       information. This debug switch causes lines to be output showing
   --       the encodings used.

   --  dc   List names of units as they are compiled. One line of output will
   --       be generated at the start of compiling each unit (package or
   --       subprogram).

   --  dd   Dynamic allocation of tables messages generated. Each time a
   --       table is reallocated, a line is output indicating the expansion.

   --  de   List the entity table

   --  df   Full tree/source print (includes withed units). Normally the tree
   --       output (dt) or recreated source output (dg,do,ds) includes only
   --       the main unit. If df is set, then the output in either case
   --       includes all compiled units (see also dg,do,ds,dt). Note that to
   --       be effective, this swich must be used in combination with one or
   --       more of dt, dg, do or ds.

   --  dg   Print the source recreated from the generated tree. In the case
   --       where the tree has been rewritten this output includes only the
   --       generated code, not the original code (see also df,do,ds,dz).
   --       This flag differs from -gnatG in that the output also includes
   --       non-source generated null statements, and freeze nodes, which
   --       are normally omitted in -gnatG mode.

   --  dh   Generates a table at the end of a compilation showing how the hash
   --       table chains built by the Namet package are loaded. This is useful
   --       in ensuring that the hashing algorithm (in Namet.Hash) is working
   --       effectively with typical sets of program identifiers.

   --  di   Generate messages for visibility linking/delinking

   --  dj   Suppress "junk null check" for access parameters. This flag permits
   --       Ada programs to pass null parameters to access parameters, and to
   --       explicitly check such access values against the null literal.
   --       Neither of these is valid Ada, but both were allowed in versions of
   --       GNAT before 3.10, so this switch can ease the transition process.

   --  dk   Immediate kill on abort. Normally on an abort (i.e. a call to
   --       Comperr.Compiler_Abort), the GNATBUG message is not given if
   --       there is a previous error. This debug switch bypasses this test
   --       and gives the message unconditionally (useful for debugging).

   --  dl   Generate unit load trace messages. A line of traceback output is
   --       generated each time a request is made to the library manager to
   --       load a new unit.

   --  dm   Some features are permitted only in OpenVMS ports of GNAT (e.g.
   --       the specification of passing by descriptor). Normally any use
   --       of these features will be flagged as an error, but this debug
   --       flag allows acceptance of these features in non OpenVMS ports.
   --       Of course they may not have any useful effect, and in particular
   --       attempting to generate code with this flag set may blow up.
   --       The flag also forces the use of 64-bits for Long_Integer.

   --  dn   Generate messages for node/list allocation. Each time a node or
   --       list header is allocated, a line of output is generated. Certain
   --       other basic tree operations also cause a line of output to be
   --       generated. This option is useful in seeing where the parser is
   --       blowing up.

   --  do   Print the source recreated from the generated tree. In the case
   --       where the tree has been rewritten, this output includes only the
   --       original code, not the generated code (see also df,dg,ds,dz).

   --  dp   Generate messages for parser scope stack push/pops. A line of
   --       output by the parser each time the parser scope stack is either
   --       pushed or popped. Useful in debugging situations where the
   --       parser scope stack ends up incorrectly synchronized

   --  dq   In layout version 1.38, 2002/01/12, a circuit was implemented
   --       to give decent default alignment to short records that had no
   --       specific alignment set. This debug option restores the previous
   --       behavior of giving such records poor alignments, typically 1.
   --       This may be useful in dealing with transition.

   --  dr   Generate parser resynchronization messages. Normally the parser
   --       resynchronizes quietly. With this debug option, two messages
   --       are generated, one when the parser starts a resynchronization
   --       skip, and another when it resumes parsing. Useful in debugging
   --       inadequate error recovery situations.

   --  ds   Print the source recreated from the generated tree. In the case
   --       where the tree has been rewritten this output includes both the
   --       generated code and the original code with the generated code
   --       being enlosed in curly brackets (see also df,do,ds,dz)

   --  dt   Print full tree. The generated tree is output (see also df,dy)

   --  du   Uncheck categorization pragmas. This debug switch causes the
   --       categorization pragmas (Pure, Preelaborate etc) to be ignored
   --       so that normal checks are not made (this is particularly useful
   --       for adding temporary debugging code to units that have pragmas
   --       that are inconsistent with the debugging code added.

   --  dv   Output trace of overload resolution. Outputs messages for
   --       overload attempts that involve cascaded errors, or where
   --       an interepretation is incompatible with the context.

   --  dw   Write semantic scope stack messages. Each time a scope is created
   --       or removed, a message is output (see the Sem_Ch8.Push_Scope and
   --       Sem_Ch8.Pop_Scope subprograms).

   --  dx   Force expansion on, even if no code being generated. Normally the
   --       expander is inhibited if no code is generated. This switch forces
   --       expansion to proceed normally even if the backend is not being
   --       called. This is particularly useful for debugging purposes when
   --       using the front-end only version of the compiler (which normally
   --       would never do any expansion).

   --  dy   Print tree of package Standard. Normally the tree print out does
   --       not include package Standard, even if the -df switch is set. This
   --       switch forces output of the internal tree built for Standard.

   --  dz   Print source of package Standard. Normally the source print out
   --       does not include package Standard, even if the -df switch is set.
   --       This switch forces output of the source recreated from the internal
   --       tree built for Standard. Note that this differs from -gnatS in
   --       that it prints from the actual tree using the normal Sprint
   --       circuitry for printing trees.

   --  dA   Forces output of representation information, including full
   --       information for all internal type and object entities, as well
   --       as all user defined type and object entities including private
   --       and incomplete types. This debug switch also automatically sets
   --       the equivalent of -gnatR3m.

   --  dB   Output debug encodings for types and variants. See Exp_Dbug for
   --       exact form of the generated output.

   --  dC   Output trace information showing the decisions made during
   --       check suppression activity in unit Checks.

   --  dD   Delete new elaboration checks. This flag causes GNAT to return
   --       to the 3.13a elaboration semantics, and to suppress the fixing
   --       of two bugs. The first is in the context of inner routines in
   --       dynamic elaboration mode, when the subprogram we are in was
   --       called at elaboration time by a unit that was also compiled with
   --       dynamic elaboration checks. In this case, if A calls B calls C,
   --       and all are in different units, we need an elaboration check at
   --       each call. These nested checks were only put in recently (see
   --       version 1.80 of Sem_Elab) and we provide this debug flag to
   --       revert to the previous behavior in case of regressions. The
   --       other behavior reverted by this flag is the treatment of the
   --       Elaborate_Body pragma in static elaboration mode. This used to
   --       be treated as not needing elaboration checking, but in fact in
   --       general Elaborate_All is still required because of nested calls.

   --  dE   Apply compile time elaboration checking for with relations between
   --       predefined units. Normally no checks are made (it seems that at
   --       least on the SGI, such checks run into trouble).

   --  dF   Front end data layout enabled. Normally front end data layout
   --       is only enabled if the target parameter Backend_Layout is False.
   --       This debugging switch enables it unconditionally.

   --  dG   Generate all warnings. Normally Errout suppresses warnings on
   --       units that are not part of the main extended source, and also
   --       suppresses warnings on instantiations in the main extended
   --       source that duplicate warnings already posted on the template.
   --       This switch stops both kinds of deletion and causes Errout to
   --       post all warnings sent to it.

   --  dH   Inhibit call to gigi. This is useful for testing front end data
   --       layout, and may be useful in other debugging situations where
   --       you do not want gigi to intefere with the testing.

   --  dI   Inhibit internal name numbering in gnatDG listing. Any sequence of
   --       the form <uppercase-letter><digits><lowercase-letter> appearing in
   --       a name is replaced by <uppercase-letter>...<lowercase-letter>. This
   --       is used in the fixed bugs run to minimize system and version
   --       dependency in filed -gnatD or -gnatG output.

   --  dJ   Generate debugging trace output for the JGNAT back end. This
   --       consists of symbolic Java Byte Code sequences for all generated
   --       classes plus additional information to indicate local variables
   --       and methods.

   --  dK   Kill all error messages. This debug flag suppresses the output
   --       of all error messages. It is used in regression tests where the
   --       error messages are target dependent and irrelevant.

   --  dL   Output trace information on elaboration checking. This debug
   --       switch causes output to be generated showing each call or
   --       instantiation as it is checked, and the progress of the recursive
   --       trace through calls at elaboration time.

   --  dM   Assume all variables have been modified, and ignore current value
   --       indications. This debug flag disconnects the tracking of constant
   --       values (see Exp_Ch2.Expand_Current_Value).

   --  dN   Do not generate file name information in exception messages

   --  dO   Output immediate error messages. This causes error messages to
   --       be output as soon as they are generated (disconnecting several
   --       circuits for improvement of messages, deletion of duplicate
   --       messages etc). Useful to diagnose compiler bombs caused by
   --       erroneous handling of error situations

   --  dP   Do not check for controlled objects in preelaborable packages.
   --       RM 10.2.1(9) forbids the use of library level controlled objects
   --       in preelaborable packages, but this restriction is a huge pain,
   --       especially in the predefined library units.

   --  dQ   Eliminate check for duplicate external tags. This check was added
   --       as per AI 0113, and causes some backward compatibility problems.
   --       It is never legitimate to have duplicate external tags, so the
   --       check is certainly valid, but this debug switch can be useful for
   --       enabling previous behavior of ignoring this problem.

   --  dR   Bypass the check for a proper version of s-rpc being present
   --       to use the -gnatz? switch. This allows debugging of the use
   --       of stubs generation without needing to have GLADE (or some
   --       other PCS  installed).

   --  dS   Omit conversion of fpt numbers to exact machine numbers in
   --       non-static evaluation contexts (see Check_Non_Static_Context).
   --       This is intended for testing out timing problems with this
   --       conversion circuit.

   --  dT   Similar to dS, but omits the conversions only in the case where
   --       the parent is not a constant declaration.

   --  dU   Enable garbage collection of unreachable entities. This enables
   --       both the reachability analysis and changing the Is_Public and
   --       Is_Eliminated flags.

   --  dV   Enable viewing of all symbols in debugger. Causes debug information
   --       to be generated for all symbols, including internal symbols. This
   --       is enabled by default for -gnatD, but this switch allows this to
   --       be enabled without generating modified source files. Note that the
   --       use of -gnatdV ensures in the dwarf/elf case that all symbols that
   --       are present in the elf tables are also in the dwarf tables (which
   --       seems to be required by some tools). Another effect of dV is to
   --       generate full qualified names, including internal names generated
   --       for blocks and loops.

   --  dW   Disable warnings when a possibly uninitialized scalar value is
   --       passed to an IN OUT parameter of a procedure. This usage is a
   --       quite improper bounded error [erroneous in Ada 83] situation,
   --       and would normally generate a warning. However, to ease the
   --       task of transitioning incorrect legacy code, we provide this
   --       undocumented feature for suppressing these warnings.

   --  dY   Enable configurable run-time mode, just as though the System file
   --       had Configurable_Run_Time_Mode set to True. This is useful in
   --       testing high integrity mode.

   --  dZ   Generate listing showing the contents of the dispatch tables. Each
   --       line has an internally generated number used for references between
   --       tagged types and primitives. For each primitive the output has the
   --       following fields:
   --
   --         - Letter 'P' or letter 's': The former indicates that this
   --           primitive will be located in a primary dispatch table. The
   --           latter indicates that it will be located in a secondary
   --           dispatch table.
   --
   --         - Name of the primitive. In case of predefined Ada primitives
   --           the text "(predefined)" is added before the name, and these
   --           acronyms are used: SR (Stream_Read), SW (Stream_Write), SI
   --           (Stream_Input), SO (Stream_Output), DA (Deep_Adjust), DF
   --           (Deep_Finalize). In addition Oeq identifies the equality
   --           operator, and "_assign" the assignment.
   --
   --         - If the primitive covers interface types, two extra fields
   --           referencing other primitives are generated: "Alias" references
   --           the primitive of the tagged type that covers an interface
   --           primitive, and "AI_Alias" references the covered interface
   --           primitive.
   --
   --         - The expression "at #xx" indicates the slot of the dispatch
   --           table occupied by such primitive in its corresponding primary
   --           or secondary dispatch table.
   --
   --         - In case of abstract subprograms the text "is abstract" is
   --           added at the end of the line.

   --  d.a  Force Target_Strict_Alignment to True, even on targets where it
   --       would normally be false. Can be used for testing strict alignment
   --       circuitry in the compiler.

   --  d.b  Dump back end types. During Create_Standard, the back end is
   --       queried for all available types. This option shows them.

   --  d.c  Generate inline concatenation, instead of calling one of the
   --       System.Concat_n.Str_Concat_n routines in cases where the latter
   --       routines would normally be called.

   --  d.d  Disable atomic synchronization for all atomic variable references.
   --       Pragma Enable_Atomic_Synchronization is ignored.

   --  d.e  Enable atomic synchronization for all atomic variable references.
   --       Pragma Disable_Atomic_Synchronization is ignored, and also the
   --       compiler switch -gnated is ignored.

   --  d.f  Suppress folding of static expressions. This of course results
   --       in seriously non-conforming behavior, but is useful sometimes
   --       when tracking down handling of complex expressions.

   --  d.g  Enables conversion of a raise statement into a goto when the
   --       relevant handler is statically determinable. For now we only try
   --       this if this debug flag is set. Later we will enable this more
   --       generally by default.

   --  d.i  Ignore all occurrences of pragma Warnings in the sources. This can
   --       be used in particular to disable Warnings (Off) to check if any of
   --       these statements are inappropriate.

   --  d.j  Generate listing of frontend inlined calls and inline calls passed
   --       to the backend. This is useful to locate skipped calls that must be
   --       inlined by the frontend.

   --  d.k  Enable new semantics of frontend inlining.  This is useful to test
   --       this new feature in all the platforms.

   --  d.l  Use Ada 95 semantics for limited function returns. This may be
   --       used to work around the incompatibility introduced by AI-318-2.
   --       It is useful only in -gnat05 mode.

   --  d.m  When -gnatl is used, the normal output includes full listings of
   --       all files in the extended main source (body/spec/subunits). If this
   --       debug switch is used, then the full listing is given only for the
   --       main source (this corresponds to a previous behavior of -gnatl and
   --       is used for running the ACATS tests).

   --  d.n  Print source file names as they are loaded. This is useful if the
   --       compiler has a bug -- these are the files that need to be included
   --       in a bug report.

   --  d.o  Generate listing showing the IL instructions generated by the .NET
   --       compiler for each subprogram.

   --  d.p  Enable the .NET CIL verifier. During development the verifier is
   --       disabled by default and this flag is used to enable it. In the
   --       future we will reverse this functionality.

   --  d.r  Forces the flag OK_To_Reorder_Components to be set in all record
   --       base types that have no discriminants.

   --  d.s  Normally the compiler expands slice moves into loops if overlap
   --       might be possible. This debug flag inhibits that expansion, and
   --       the back end is expected to use an appropriate routine to handle
   --       overlap, based on Forward_OK and Backwards_OK flags.

   --  d.t  The compiler has been modified (a fairly extensive modification)
   --       to generate static dispatch tables for library level tagged types.
   --       This debug switch disables this modification and reverts to the
   --       previous dynamic construction of tables. It is there as a possible
   --       work around if we run into trouble with the new implementation.

   --  d.u  Sets Modify_Tree_For_C mode in which tree is modified to make it
   --       easier to generate code using a C compiler.

   --  d.v  Forces the flag OK_To_Reorder_Components to be set in all record
   --       base types that have at least one discriminant (v = variant).

   --  d.w  This flag turns off the scanning of loops to detect possible
   --       infinite loops.

   --  d.x  No exception handlers in generated code. This causes exception
   --       handlers to be eliminated from the generated code. They are still
   --       fully compiled and analyzed, they just get eliminated from the
   --       code generation step.

   --  d.z  Temporary debug switch for control of the why non-static messages
   --       generated by Why_Non_Static. Normally these messages are suppressed
   --       in ASIS mode (d2), but if d.z is set they are not suppressed. This
   --       is a temporary switch to aid in updating ASIS base lines.

   --  d.A  There seems to be a problem with ASIS if we activate the circuit
   --       for reading and writing the aspect specification hash table, so
   --       for now, this is controlled by the debug flag d.A. The hash table
   --       is only written and read if this flag is set.

   --  d.C  Generate call to System.Concat_n.Str_Concat_n routines in cases
   --       where we would normally generate inline concatenation code.

   --  d.E  Turn selected errors into warnings. This debug switch causes a
   --       specific set of error messages into warnings. Setting this switch
   --       causes Opt.Error_To_Warning to be set to True. The intention is
   --       that this be used for messages representing upwards incompatible
   --       changes to Ada 2012 that cause previously correct programs to be
   --       treated as illegal now. The following cases are affected:
   --
   --          Errors relating to overlapping subprogram parameters for cases
   --          other than IN OUT parameters to functions.
   --
   --          Errors relating to the new rules about not defining equality
   --          too late so that composition of equality can be assured.
   --
   --          Errors relating to overriding indicators on protected subprogram
   --          bodies (not an Ada 2012 incompatibility, but might cause errors
   --          for existing programs assuming they were legal because GNAT
   --          formerly allowed them).

   --  d.F  Sets GNATprove_Mode to True. This allows debugging the frontend in
   --       the special mode used by GNATprove.

   --  d.G  Previously the compiler ignored calls via generic formal parameters
   --       when doing the analysis for the static elaboration model. This is
   --       now fixed, but we provide this debug flag to revert to the previous
   --       situation of ignoring such calls to aid in transition.

   --  d.I  Do not ignore enum representation clauses in CodePeer mode.
   --       The default of ignoring representation clauses for enumeration
   --       types in CodePeer is good for the majority of Ada code, but in some
   --       cases being able to change this default might be useful to remove
   --       some false positives.

   --  d.J  Disable parallel SCIL generation. Normally SCIL file generation is
   --       done in parallel to speed processing. This switch disables this
   --       behavior.

   --  d.L  Normally the front end generates special expansion for conditional
   --       expressions of a limited type. This debug flag removes this special
   --       case expansion, leaving it up to the back end to handle conditional
   --       expressions correctly.

   --  d.M  Relaxed RM semantics. This flag sets Opt.Relaxed_RM_Semantics
   --       See Opt.Relaxed_RM_Semantics for more details.

   --  d.N  Enlarge entities by one node (but don't attempt to use this extra
   --       node for storage of any flags or fields). This can be used to do
   --       experiments on the impact of increasing entity sizes.

   --  d.O  Dump internal SCO tables. Before outputting the SCO information to
   --       the ALI file, the internal SCO tables (SCO_Table/SCO_Unit_Table)
   --       are dumped for debugging purposes.

   --  d.P  Previous non-optimized handling of length comparisons. Setting this
   --       flag inhibits the effect of Optimize_Length_Comparison in Exp_Ch4.
   --       This is there in case we find a situation where the optimization
   --       malfunctions, to provide a work around.

   --  d.R  As documented in lib-writ.ads, restrictions in the ali file can
   --       have two forms, positional and named. The named notation is the
   --       current preferred form, but the use of this debug switch will force
   --       the use of the obsolescent positional form.

   --  d.S  Force Optimize_Alignment (Space) mode as the default

   --  d.T  Force Optimize_Alignment (Time) mode as the default

   --  d.U  Ignore indirect calls for static elaboration. The static
   --       elaboration model is conservative, especially regarding indirect
   --       calls. If you say Proc'Access, it will assume you might call
   --       Proc. This can cause elaboration cycles at bind time. This flag
   --       reverts to the behavior of earlier compilers, which ignored
   --       indirect calls.

   --  d.W  Print out debugging information for Walk_Library_Items, including
   --       the order in which units are walked. This is primarily for use in
   --       debugging CodePeer mode.

   --  d1   Error messages have node numbers where possible. Normally error
   --       messages have only source locations. This option is useful when
   --       debugging errors caused by expanded code, where the source location
   --       does not give enough information.

   --  d2   Suppress output of the error position flags for verbose form error
   --       messages. The messages are still interspersed in the listing, but
   --       without any error flags or extra blank lines. Also causes an extra
   --       <<< to be output at the right margin. This is intended to be the
   --       easiest format for checking conformance of ACATS B tests. This
   --       flag also suppresses the additional messages explaining why a
   --       non-static expression is non-static (see Sem_Eval.Why_Not_Static).
   --       This avoids having to worry about these messages in ACATS testing.

   --  d3   Causes Comperr to dump the contents of the node for which an abort
   --       was detected (normally only the Node_Id of the node is output).

   --  d4   Inhibits automatic krunching of predefined library unit file names.
   --       Normally, as described in the spec of package Krunch, such files
   --       are automatically krunched to 8 characters, with special treatment
   --       of the prefixes Ada, System, and Interfaces. Setting this debug
   --       switch disables this special treatment.

   --  d5   Causes the tree read/write circuit to output detailed information
   --       tracking the data that is read and written element by element.

   --  d6   Normally access-to-unconstrained-array types are represented
   --       using fat (double) pointers. Using this debug flag causes them
   --       to default to thin. This can be used to test the performance
   --       implications of using thin pointers, and also to test that the
   --       compiler functions correctly with this choice.

   --  d7   Normally a -gnatl or -gnatv listing includes the time stamp
   --       of the source file. This debug flag suppresses this output,
   --       and also suppresses the message with the version number.
   --       This is useful in certain regression tests.

   --  d8   This forces the packed stuff to generate code assuming the
   --       opposite endianness from the actual correct value. Useful in
   --       testing out code generation from the packed routines.

   --  d9   This allows lock free implementation for protected objects
   --       (see Exp_Ch9).

   ------------------------------------------
   -- Documentation for Binder Debug Flags --
   ------------------------------------------

   --  da  Normally if there is an elaboration circularity, then in describing
   --      the cycle, links involving internal units are omitted, since they
   --      are irrelevant and confusing. This debug flag causes all links to
   --      be listed, and is useful when diagnosing circularities introduced
   --      by incorrect changes to the run-time library itself.

   --  db  Output debug information from Better_Choice in Binde, which uses
   --      various heuristics to determine elaboration order in cases where
   --      multiple orders are valid.

   --  dc  List units as they are chosen. As units are selected for addition to
   --      the elaboration order, a line of output is generated showing which
   --      unit has been selected.

   --  de  Similar to the effect of -e (output complete list of elaboration
   --      dependencies) except that internal units are included in the
   --      listing.

   --  di  Normally gnatbind calls Read_Ali with Ignore_Errors set to
   --      False, since the binder really needs correct version ALI
   --      files to do its job. This debug flag causes Ignore_Errors
   --      mode to be set for the binder (and is particularly useful
   --      for testing ignore errors mode).

   --  dn  List details of manipulation of Num_Pred values during execution of
   --      the algorithm used to determine a correct order of elaboration. This
   --      is useful in diagnosing any problems in its behavior.

   --  do  Use old elaboration order preference. The new preference rules
   --      prefer specs with no bodies to specs with bodies, and between two
   --      specs with bodies, prefers the one whose body is closer to being
   --      able to be elaborated. This is a clear improvement, but we provide
   --      this debug flag in case of regressions.

   --  du  List unit name and file name for each unit as it is read in

   --  dx  Force the binder to read (and then ignore) the xref information
   --      in ali files (used to check that read circuit is working OK).

   --------------------------------------------
   -- Documentation for gnatmake Debug Flags --
   --------------------------------------------

   --  df  Only output file names, not path names, in log

   --  dh  Generate listing showing loading of name table hash chains,
   --      same as for the compiler.

   --  dm  Issue a message indicating the maximum number of simultaneous
   --      compilations.

   --  dn  Do not delete temporary files created by gnatmake at the end
   --      of execution, such as temporary config pragma files, mapping
   --      files or project path files.

   --  dp  Prints the Q used by routine Make.Compile_Sources every time
   --      we go around the main compile loop of Make.Compile_Sources

   --  dq  Prints source files as they are enqueued and dequeued in the Q
   --      used by routine Make.Compile_Sources. Useful to figure out the
   --      order in which sources are recompiled.

   --  dt  When a time stamp mismatch has been found for an ALI file,
   --      display the source file name, the time stamp expected and
   --      the time stamp found.

   --  du  List unit name and file name for each unit as it is read in

   --  dw  Prints the list of units withed by the unit currently explored
   --      during the main loop of Make.Compile_Sources.

   ---------------------------------------------
   -- Documentation for gprbuild Debug Flags  --
   ---------------------------------------------

   --  dn  Do not delete temporary files createed by gprbuild at the end
   --      of execution, such as temporary config pragma files, mapping
   --      files or project path files.

   --  dt  When a time stamp mismatch has been found for an ALI file,
   --      display the source file name, the time stamp expected and
   --      the time stamp found.

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True) is
      subtype Dig  is Character range '1' .. '9';
      subtype LLet is Character range 'a' .. 'z';
      subtype ULet is Character range 'A' .. 'Z';

   begin
      if C in Dig then
         case Dig (C) is
            when '1' => Debug_Flag_1 := Val;
            when '2' => Debug_Flag_2 := Val;
            when '3' => Debug_Flag_3 := Val;
            when '4' => Debug_Flag_4 := Val;
            when '5' => Debug_Flag_5 := Val;
            when '6' => Debug_Flag_6 := Val;
            when '7' => Debug_Flag_7 := Val;
            when '8' => Debug_Flag_8 := Val;
            when '9' => Debug_Flag_9 := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' => Debug_Flag_AA := Val;
            when 'B' => Debug_Flag_BB := Val;
            when 'C' => Debug_Flag_CC := Val;
            when 'D' => Debug_Flag_DD := Val;
            when 'E' => Debug_Flag_EE := Val;
            when 'F' => Debug_Flag_FF := Val;
            when 'G' => Debug_Flag_GG := Val;
            when 'H' => Debug_Flag_HH := Val;
            when 'I' => Debug_Flag_II := Val;
            when 'J' => Debug_Flag_JJ := Val;
            when 'K' => Debug_Flag_KK := Val;
            when 'L' => Debug_Flag_LL := Val;
            when 'M' => Debug_Flag_MM := Val;
            when 'N' => Debug_Flag_NN := Val;
            when 'O' => Debug_Flag_OO := Val;
            when 'P' => Debug_Flag_PP := Val;
            when 'Q' => Debug_Flag_QQ := Val;
            when 'R' => Debug_Flag_RR := Val;
            when 'S' => Debug_Flag_SS := Val;
            when 'T' => Debug_Flag_TT := Val;
            when 'U' => Debug_Flag_UU := Val;
            when 'V' => Debug_Flag_VV := Val;
            when 'W' => Debug_Flag_WW := Val;
            when 'X' => Debug_Flag_XX := Val;
            when 'Y' => Debug_Flag_YY := Val;
            when 'Z' => Debug_Flag_ZZ := Val;
         end case;

      else
         case LLet (C) is
            when 'a' => Debug_Flag_A := Val;
            when 'b' => Debug_Flag_B := Val;
            when 'c' => Debug_Flag_C := Val;
            when 'd' => Debug_Flag_D := Val;
            when 'e' => Debug_Flag_E := Val;
            when 'f' => Debug_Flag_F := Val;
            when 'g' => Debug_Flag_G := Val;
            when 'h' => Debug_Flag_H := Val;
            when 'i' => Debug_Flag_I := Val;
            when 'j' => Debug_Flag_J := Val;
            when 'k' => Debug_Flag_K := Val;
            when 'l' => Debug_Flag_L := Val;
            when 'm' => Debug_Flag_M := Val;
            when 'n' => Debug_Flag_N := Val;
            when 'o' => Debug_Flag_O := Val;
            when 'p' => Debug_Flag_P := Val;
            when 'q' => Debug_Flag_Q := Val;
            when 'r' => Debug_Flag_R := Val;
            when 's' => Debug_Flag_S := Val;
            when 't' => Debug_Flag_T := Val;
            when 'u' => Debug_Flag_U := Val;
            when 'v' => Debug_Flag_V := Val;
            when 'w' => Debug_Flag_W := Val;
            when 'x' => Debug_Flag_X := Val;
            when 'y' => Debug_Flag_Y := Val;
            when 'z' => Debug_Flag_Z := Val;
         end case;
      end if;
   end Set_Debug_Flag;

   ---------------------------
   -- Set_Dotted_Debug_Flag --
   ---------------------------

   procedure Set_Dotted_Debug_Flag (C : Character; Val : Boolean := True) is
      subtype Dig  is Character range '1' .. '9';
      subtype LLet is Character range 'a' .. 'z';
      subtype ULet is Character range 'A' .. 'Z';

   begin
      if C in Dig then
         case Dig (C) is
            when '1' => Debug_Flag_Dot_1 := Val;
            when '2' => Debug_Flag_Dot_2 := Val;
            when '3' => Debug_Flag_Dot_3 := Val;
            when '4' => Debug_Flag_Dot_4 := Val;
            when '5' => Debug_Flag_Dot_5 := Val;
            when '6' => Debug_Flag_Dot_6 := Val;
            when '7' => Debug_Flag_Dot_7 := Val;
            when '8' => Debug_Flag_Dot_8 := Val;
            when '9' => Debug_Flag_Dot_9 := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' => Debug_Flag_Dot_AA := Val;
            when 'B' => Debug_Flag_Dot_BB := Val;
            when 'C' => Debug_Flag_Dot_CC := Val;
            when 'D' => Debug_Flag_Dot_DD := Val;
            when 'E' => Debug_Flag_Dot_EE := Val;
            when 'F' => Debug_Flag_Dot_FF := Val;
            when 'G' => Debug_Flag_Dot_GG := Val;
            when 'H' => Debug_Flag_Dot_HH := Val;
            when 'I' => Debug_Flag_Dot_II := Val;
            when 'J' => Debug_Flag_Dot_JJ := Val;
            when 'K' => Debug_Flag_Dot_KK := Val;
            when 'L' => Debug_Flag_Dot_LL := Val;
            when 'M' => Debug_Flag_Dot_MM := Val;
            when 'N' => Debug_Flag_Dot_NN := Val;
            when 'O' => Debug_Flag_Dot_OO := Val;
            when 'P' => Debug_Flag_Dot_PP := Val;
            when 'Q' => Debug_Flag_Dot_QQ := Val;
            when 'R' => Debug_Flag_Dot_RR := Val;
            when 'S' => Debug_Flag_Dot_SS := Val;
            when 'T' => Debug_Flag_Dot_TT := Val;
            when 'U' => Debug_Flag_Dot_UU := Val;
            when 'V' => Debug_Flag_Dot_VV := Val;
            when 'W' => Debug_Flag_Dot_WW := Val;
            when 'X' => Debug_Flag_Dot_XX := Val;
            when 'Y' => Debug_Flag_Dot_YY := Val;
            when 'Z' => Debug_Flag_Dot_ZZ := Val;
         end case;

      else
         case LLet (C) is
            when 'a' => Debug_Flag_Dot_A := Val;
            when 'b' => Debug_Flag_Dot_B := Val;
            when 'c' => Debug_Flag_Dot_C := Val;
            when 'd' => Debug_Flag_Dot_D := Val;
            when 'e' => Debug_Flag_Dot_E := Val;
            when 'f' => Debug_Flag_Dot_F := Val;
            when 'g' => Debug_Flag_Dot_G := Val;
            when 'h' => Debug_Flag_Dot_H := Val;
            when 'i' => Debug_Flag_Dot_I := Val;
            when 'j' => Debug_Flag_Dot_J := Val;
            when 'k' => Debug_Flag_Dot_K := Val;
            when 'l' => Debug_Flag_Dot_L := Val;
            when 'm' => Debug_Flag_Dot_M := Val;
            when 'n' => Debug_Flag_Dot_N := Val;
            when 'o' => Debug_Flag_Dot_O := Val;
            when 'p' => Debug_Flag_Dot_P := Val;
            when 'q' => Debug_Flag_Dot_Q := Val;
            when 'r' => Debug_Flag_Dot_R := Val;
            when 's' => Debug_Flag_Dot_S := Val;
            when 't' => Debug_Flag_Dot_T := Val;
            when 'u' => Debug_Flag_Dot_U := Val;
            when 'v' => Debug_Flag_Dot_V := Val;
            when 'w' => Debug_Flag_Dot_W := Val;
            when 'x' => Debug_Flag_Dot_X := Val;
            when 'y' => Debug_Flag_Dot_Y := Val;
            when 'z' => Debug_Flag_Dot_Z := Val;
         end case;
      end if;
   end Set_Dotted_Debug_Flag;

end Debug;
