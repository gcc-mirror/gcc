------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                D E B U G                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
   --  dm   Prevent special frontend inlining in GNATprove mode
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
   --  dF
   --  dG   Generate all warnings including those normally suppressed
   --  dH   Hold (kill) call to gigi
   --  dI   Inhibit internal name numbering in gnatG listing
   --  dJ   Prepend subprogram name in messages
   --  dK   Kill all error messages
   --  dL   Ignore external calls from instances for elaboration
   --  dM   Assume all variables are modified (no current values)
   --  dN   No file name information in exception messages
   --  dO   Output immediate error messages
   --  dP   Do not check for controlled objects in preelaborable packages
   --  dQ   Use old secondary stack method
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
   --  d.h  Minimize the creation of public internal symbols for concatenation
   --  d.i  Ignore Warnings pragmas
   --  d.j  Generate listing of frontend inlined calls
   --  d.k  Kill referenced run-time library unit line numbers
   --  d.l  Use Ada 95 semantics for limited function returns
   --  d.m  For -gnatl, print full source only for main unit
   --  d.n  Print source file names
   --  d.o  Conservative elaboration order for indirect calls
   --  d.p  Use original Ada 95 semantics for Bit_Order (disable AI95-0133)
   --  d.q  Suppress optimizations on imported 'in'
   --  d.r  Disable reordering of components in record types
   --  d.s  Strict secondary stack management
   --  d.t  Disable static allocation of library level dispatch tables
   --  d.u  Enable Modify_Tree_For_C (update tree for c)
   --  d.v  Enforce SPARK elaboration rules in SPARK code
   --  d.w  Do not check for infinite loops
   --  d.x  No exception handlers
   --  d.y  Disable implicit pragma Elaborate_All on task bodies
   --  d.z  Restore previous support for frontend handling of Inline_Always

   --  d.A
   --  d.B  Generate a bug box on abort_statement
   --  d.C  Generate concatenation call, do not generate inline code
   --  d.D  Disable errors on use of overriding keyword in Ada 95 mode
   --  d.E  Turn selected errors into warnings
   --  d.F  Debug mode for GNATprove
   --  d.G  Ignore calls through generic formal parameters for elaboration
   --  d.H
   --  d.I  Do not ignore enum representation clauses in CodePeer mode
   --  d.J  Relaxed rules for pragma No_Return
   --  d.K  Do not reject components in extensions overlapping with parent
   --  d.L  Depend on back end for limited types in if and case expressions
   --  d.M  Relaxed RM semantics
   --  d.N  Add node to all entities
   --  d.O  Dump internal SCO tables
   --  d.P  Previous (non-optimized) handling of length comparisons
   --  d.Q  Previous (incomplete) style check for binary operators
   --  d.R  Restrictions in ali files in positional form
   --  d.S  Force Optimize_Alignment (Space)
   --  d.T  Force Optimize_Alignment (Time)
   --  d.U  Ignore indirect calls for static elaboration
   --  d.V  Do not verify validity of SCIL files (CodePeer mode)
   --  d.W  Print out debugging information for Walk_Library_Items
   --  d.X  Old treatment of indexing aspects
   --  d.Y
   --  d.Z  Do not enable expansion in configurable run-time mode

   --  d_a  Stop elaboration checks on accept or select statement
   --  d_b
   --  d_c
   --  d_d
   --  d_e  Ignore entry calls and requeue statements for elaboration
   --  d_f  Issue info messages related to GNATprove usage
   --  d_g
   --  d_h
   --  d_i  Ignore activations and calls to instances for elaboration
   --  d_j  Read JSON files and populate Repinfo tables (opposite of -gnatRjs)
   --  d_k
   --  d_l
   --  d_m
   --  d_n
   --  d_o
   --  d_p  Ignore assertion pragmas for elaboration
   --  d_q
   --  d_r
   --  d_s  Stop elaboration checks on synchronous suspension
   --  d_t
   --  d_u
   --  d_v
   --  d_w
   --  d_x
   --  d_y
   --  d_z  Enable Put_Image on tagged types

   --  d_A  Stop generation of ALI file
   --  d_B  Warn on build-in-place function calls
   --  d_C
   --  d_D
   --  d_E
   --  d_F  Encode full invocation paths in ALI files
   --  d_G
   --  d_H
   --  d_I
   --  d_J
   --  d_K  (Reserved) Enable reporting a warning on known-problem issues
   --  d_L  Output trace information on elaboration checking
   --  d_M
   --  d_N
   --  d_O
   --  d_P
   --  d_Q
   --  d_R
   --  d_S
   --  d_T  Output trace information on invocation path recording
   --  d_U
   --  d_V  Enable verifications on the expanded tree
   --  d_W
   --  d_X
   --  d_Y
   --  d_Z

   --  d1   Error msgs have node numbers where possible
   --  d2   Eliminate error flags in verbose form error messages
   --  d3   Dump bad node in Comperr on an abort
   --  d4   Inhibit automatic krunch of predefined library unit files
   --  d5   Debug output for tree read/write
   --  d6   Default access unconstrained to thin pointers
   --  d7   Suppress version/source stamp/compilation time for -gnatv/-gnatl
   --  d8   Force opposite endianness in packed stuff
   --  d9   Allow lock free implementation

   --  d.1  Enable unnesting of nested procedures
   --  d.2  Allow statements in declarative part
   --  d.3  Output debugging information from Exp_Unst
   --  d.4  Do not delete generated C file in case of errors
   --  d.5  Do not generate imported subprogram definitions in C code
   --  d.6  Do not avoid declaring unreferenced types in C code
   --  d.7  Disable unsound heuristics in gnat2scil (for CP as SPARK prover)
   --  d.8
   --  d.9  Disable build-in-place for nonlimited types

   --  d_1
   --  d_2
   --  d_3
   --  d_4
   --  d_5
   --  d_6
   --  d_7
   --  d_8
   --  d_9

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
   --  do  Use older preference for elaboration order
   --  dp  Use old preference for elaboration order
   --  dq
   --  dr
   --  ds
   --  dt
   --  du  List units as they are acquired
   --  dv  Verbose debugging printouts
   --  dw
   --  dx  Force binder to read xref information from ali files
   --  dy
   --  dz

   --  dA
   --  dB
   --  dC
   --  dD
   --  dE
   --  dF
   --  dG
   --  dH
   --  dI
   --  dJ
   --  dK
   --  dL
   --  dM
   --  dN
   --  dO
   --  dP
   --  dQ
   --  dR
   --  dS
   --  dT
   --  dU
   --  dV
   --  dW
   --  dX
   --  dY
   --  dZ

   --  d.a
   --  d.b
   --  d.c
   --  d.d
   --  d.e
   --  d.f
   --  d.g
   --  d.h
   --  d.i
   --  d.j
   --  d.k
   --  d.l
   --  d.m
   --  d.n
   --  d.o
   --  d.p
   --  d.q
   --  d.r
   --  d.s
   --  d.t
   --  d.u
   --  d.v
   --  d.w
   --  d.x
   --  d.y
   --  d.z

   --  d.A
   --  d.B
   --  d.C
   --  d.D
   --  d.E
   --  d.F
   --  d.G
   --  d.H
   --  d.I
   --  d.J
   --  d.K
   --  d.L
   --  d.M
   --  d.N
   --  d.O
   --  d.P
   --  d.Q
   --  d.R
   --  d.S
   --  d.T
   --  d.U
   --  d.V
   --  d.W
   --  d.X
   --  d.Y
   --  d.Z

   --  d.1
   --  d.2
   --  d.3
   --  d.4
   --  d.5
   --  d.6
   --  d.7
   --  d.8
   --  d.9

   --  d_a  Ignore the effects of pragma Elaborate_All
   --  d_b  Ignore the effects of pragma Elaborate_Body
   --  d_c
   --  d_d
   --  d_e  Ignore the effects of pragma Elaborate
   --  d_f
   --  d_g
   --  d_h
   --  d_i
   --  d_j
   --  d_k
   --  d_l
   --  d_m
   --  d_n
   --  d_o
   --  d_p
   --  d_q
   --  d_r
   --  d_s
   --  d_t  Output cycle-detection trace information
   --  d_u
   --  d_v
   --  d_w
   --  d_x
   --  d_y
   --  d_z

   --  d_A  Output ALI invocation tables
   --  d_B
   --  d_C  Diagnose all cycles
   --  d_D
   --  d_E
   --  d_F
   --  d_G
   --  d_H
   --  d_I  Output invocation graph
   --  d_J
   --  d_K
   --  d_L  Output library graph
   --  d_M
   --  d_N
   --  d_O
   --  d_P  Output cycle paths
   --  d_Q
   --  d_R
   --  d_S  Output elaboration-order status
   --  d_T  Output elaboration-order trace information
   --  d_U
   --  d_V  Validate bindo cycles, graphs, and order
   --  d_W
   --  d_X
   --  d_Y
   --  d_Z

   --  d_1
   --  d_2
   --  d_3
   --  d_4
   --  d_5
   --  d_6
   --  d_7
   --  d_8
   --  d_9

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
   --       be effective, this switch must be used in combination with one or
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

   --  dm   Prevent special frontend inlining in GNATprove mode. In some cases,
   --       some subprogram calls are inlined in GNATprove mode in order to
   --       facilitate formal verification. This debug switch prevents that
   --       inlining to happen.

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
   --       elaboration control pragmas (Pure, Preelaborate, etc.) and the
   --       categorization pragmas (Shared_Passive, Remote_Types, etc.) to be
   --       ignored, so that normal checks are not made (this is particularly
   --       useful for adding temporary debugging code to units that have
   --       pragmas that are inconsistent with the debugging code added).

   --  dv   Output trace of overload resolution. Outputs messages for
   --       overload attempts that involve cascaded errors, or where
   --       an interpretation is incompatible with the context.

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
   --       the equivalent of -gnatRm.

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
   --       predefined units. Normally no checks are made.

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

   --  dJ   Prepend the name of the enclosing subprogram in compiler messages
   --       (errors, warnings, style checks). This is useful in particular to
   --       integrate compiler warnings in static analysis tools such as
   --       CodePeer.

   --  dK   Kill all error messages. This debug flag suppresses the output
   --       of all error messages. It is used in regression tests where the
   --       error messages are target dependent and irrelevant.

   --  dL   The compiler ignores calls in instances and invoke subprograms
   --       which are external to the instance for both the static and dynamic
   --       elaboration models.

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

   --  dQ   Use old method for determining what goes on the secondary stack.
   --       This disables some newer optimizations. The intent is to use this
   --       temporarily to measure before/after efficiency. ???Remove this
   --       when we are done (see Sem_Util.Requires_Transient_Scope).

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

   --  d.h  Minimize the creation of public internal symbols for concatenation
   --       by enforcing a secondary stack-like handling of the final result.
   --       The target of the concatenation is thus constrained in place and
   --       initialized with the result instead of acting as its alias.

   --  d.i  Ignore all occurrences of pragma Warnings in the sources. This can
   --       be used in particular to disable Warnings (Off) to check if any of
   --       these statements are inappropriate.

   --  d.k  If an error message contains a reference to a location in an
   --       internal unit, then suppress the line number in this reference.

   --  d.j  Generate listing of frontend inlined calls and inline calls passed
   --       to the backend. This is useful to locate skipped calls that must be
   --       inlined by the frontend.

   --  d.l  Use Ada 95 semantics for limited function returns. This may be
   --       used to work around the incompatibility introduced by AI-318-2.
   --       It is useful only in Ada 2005 and later.

   --  d.m  When -gnatl is used, the normal output includes full listings of
   --       all files in the extended main source (body/spec/subunits). If this
   --       debug switch is used, then the full listing is given only for the
   --       main source (this corresponds to a previous behavior of -gnatl and
   --       is used for running the ACATS tests).

   --  d.n  Print source file names as they are loaded. This is useful if the
   --       compiler has a bug -- these are the files that need to be included
   --       in a bug report.

   --  d.o  Conservative elaboration order for indirect calls. This causes
   --       P'Access to be treated as a call in more cases.

   --  d.p  In Ada 95 (or 83) mode, use original Ada 95 behavior for the
   --       interpretation of component clauses crossing byte boundaries when
   --       using the non-default bit order (i.e. ignore AI95-0133).

   --  d.q  If an array variable or constant is not modified in Ada code, and
   --       is passed to an 'in' parameter of a foreign-convention subprogram,
   --       and that subprogram modifies the array, the Ada compiler normally
   --       assumes that the array is not modified. This option suppresses such
   --       optimizations. This option should not be used; the correct solution
   --       is to declare the parameter 'in out'.

   --  d.r  Do not reorder components in record types.

   --  d.s  The compiler no longer attempts to optimize the calls to secondary
   --       stack management routines SS_Mark and SS_Release. As a result, each
   --       transient block tasked with secondary stack management will fulfill
   --       its role unconditionally.

   --  d.s  The compiler does not generate calls to secondary stack management
   --       routines SS_Mark and SS_Release for a transient block when there is
   --       an enclosing scoping construct which already manages the secondary
   --       stack.

   --  d.t  The compiler has been modified (a fairly extensive modification)
   --       to generate static dispatch tables for library level tagged types.
   --       This debug switch disables this modification and reverts to the
   --       previous dynamic construction of tables. It is there as a possible
   --       work around if we run into trouble with the new implementation.

   --  d.u  Sets Modify_Tree_For_C mode in which tree is modified to make it
   --       easier to generate code using a C compiler.

   --  d.v  This flag enforces the elaboration rules defined in the SPARK
   --       Reference Manual, chapter 7.7, to all SPARK code within a unit. As
   --       a result, constructs which violate the rules in chapter 7.7 are no
   --       longer accepted, even if the implementation is able to statically
   --       ensure that accepting these constructs does not introduce the
   --       possibility of failing an elaboration check.

   --  d.w  This flag turns off the scanning of loops to detect possible
   --       infinite loops.

   --  d.x  No exception handlers in generated code. This causes exception
   --       handlers to be eliminated from the generated code. They are still
   --       fully compiled and analyzed, they just get eliminated from the
   --       code generation step.

   --  d.y  Disable implicit pragma Elaborate_All on task bodies. When a task
   --       body calls a procedure in the same package, and that procedure
   --       calls a procedure in another package, the static elaboration
   --       machinery adds an implicit Elaborate_All on the other package. This
   --       switch disables the addition of the implicit pragma in such cases.

   --  d.z  Restore previous front-end support for Inline_Always. In default
   --       mode, for targets that use the GCC back end, Inline_Always is
   --       handled by the back end. Use of this switch restores the previous
   --       handling of Inline_Always by the front end on such targets. For the
   --       targets that do not use the GCC back end, this switch is ignored.

   --  d.B  Generate a bug box when we see an abort_statement, even though
   --       there is no bug. Useful for testing Comperr.Compiler_Abort: write
   --       some code containing an abort_statement, and compile it with
   --       -gnatd.B. There is nothing special about abort_statements; it just
   --       provides a way to control where the bug box is generated. See "when
   --       N_Abort_Statement" in package body Expander.

   --  d.C  Generate call to System.Concat_n.Str_Concat_n routines in cases
   --       where we would normally generate inline concatenation code.

   --  d.D  For compatibility with some Ada 95 compilers implementing only
   --       one feature of Ada 2005 (overriding keyword), disable errors on use
   --       of overriding keyword in Ada 95 mode.

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

   --  d.J  Relaxed rules for pragma No_Return. A pragma No_Return is illegal
   --       if it applies to a body. This switch disables the legality check
   --       for that. If the procedure does in fact return normally, execution
   --       is erroneous, and therefore unpredictable.

   --  d.K  Do not reject components in extensions overlapping with the parent
   --       component. Such components can be specified by means of a component
   --       clause but they cannot be fully supported by the GCC type system.
   --       This switch nevertheless allows them for the sake of compatibility.

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

   --  d.Q  Previous incomplete style checks for binary operators. Style checks
   --       for token separation rules were incomplete and have been made
   --       compliant with the documentation. For example, no warning was
   --       issued for expressions such as 16-One or "A"&"B". Setting this flag
   --       inhibits these new checks.

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

   --  d.V  Do not verify the validity of SCIL files (CodePeer mode). When
   --       generating SCIL files for CodePeer, by default we verify that the
   --       SCIL is well formed before saving it on disk. This switch can be
   --       used to disable this checking, either to improve speed or to shut
   --       down a false positive detected during the verification.

   --  d.W  Print out debugging information for Walk_Library_Items, including
   --       the order in which units are walked. This is primarily for use in
   --       debugging CodePeer mode.

   --  d.X  A previous version of GNAT allowed indexing aspects to be redefined
   --       on derived container types, while the default iterator was
   --       inherited from the parent type. This nonstandard extension is
   --       preserved temporarily for use by the modeling project under debug
   --       flag d.X.

   --  d.Z  Normally we always enable expansion in configurable run-time mode
   --       to make sure we get error messages about unsupported features even
   --       when compiling in -gnatc mode. But expansion is turned off in this
   --       case if debug flag -gnatd.Z is used. This is to deal with the case
   --       where we discover difficulties in this new processing.

   --  d_a  The compiler stops the examination of a task body once it reaches
   --       an accept or select statement for the static elaboration model. The
   --       behavior is similar to that of No_Entry_Calls_In_Elaboration_Code,
   --       but does not penalize actual entry calls in elaboration code.

   --  d_e  The compiler ignores simple entry calls, asynchronous transfer of
   --       control, conditional entry calls, timed entry calls, and requeue
   --       statements in both the static and dynamic elaboration models.

   --  d_f  Issue info messages related to GNATprove usage to help users
   --       understand analysis results. By default these are not issued as
   --       beginners find them confusing. Set automatically by GNATprove when
   --       switch --info is used.

   --  d_i  The compiler ignores calls and task activations when they target a
   --       subprogram or task type defined in an external instance for both
   --       the static and dynamic elaboration models.

   --  d_j  The compiler reads JSON files that would be generated by the same
   --       compilation session if -gnatRjs was passed, in order to populate
   --       the internal tables of the Repinfo unit from them.

   --  d_p  The compiler ignores calls to subprograms which verify the run-time
   --       semantics of invariants and postconditions in both the static and
   --       dynamic elaboration models.

   --  d_s  The compiler stops the examination of a task body once it reaches
   --       a call to routine Ada.Synchronous_Task_Control.Suspend_Until_True
   --       or Ada.Synchronous_Barriers.Wait_For_Release.

   --  d_z  Enable the default Put_Image on tagged types that are not
   --       predefined.

   --  d_A  Do not generate ALI files by setting Opt.Disable_ALI_File.

   --  d_B  Warn on build-in-place function calls. This allows users to
   --       inspect their code in case it triggers compiler bugs related
   --       to build-in-place calls. See known-problem entries for details.

   --  d_F  The compiler encodes the full path from an invocation construct to
   --       an external target, offering additional information to GNATBIND for
   --       purposes of error diagnostics.

   --  d_K  (Reserved) Enable reporting a warning on known-problem issues of
   --       previous releases. No action performed in the wavefront.

   --  d_L  Output trace information on elaboration checking. This debug switch
   --       causes output to be generated showing each call or instantiation as
   --       it is checked, and the progress of the recursive trace through
   --       elaboration calls at compile time.

   --  d_T  The compiler outputs trace information to standard output whenever
   --       an invocation path is recorded.

   --  d_V  Enable verification of the expanded code before calling the backend
   --       and generate error messages on each inconsistency found.

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

   --  d7   Normally a -gnatl or -gnatv listing includes the time stamp of the
   --       source file and the time of the compilation. This debug flag can
   --       be used to suppress this output, and also suppresses the message
   --       with the version of the compiler. This is useful for regression
   --       tests which need to have consistent output.

   --  d8   This forces the packed stuff to generate code assuming the
   --       opposite endianness from the actual correct value. Useful in
   --       testing out code generation from the packed routines.

   --  d9   This allows lock free implementation for protected objects
   --       (see Exp_Ch9).

   --  d.1  Sets Opt.Unnest_Subprogram_Mode to enable unnesting of subprograms.
   --       This special pass does not actually unnest things, but it ensures
   --       that a nested procedure does not contain any uplevel references.
   --       See spec of Exp_Unst for full details.

   --  d.2  Allow statements within declarative parts. This is not usually
   --       allowed, but in some debugging contexts (e.g. testing the circuit
   --       for unnesting of procedures), it is useful to allow this.

   --  d.3  Output debugging information from Exp_Unst, including the name of
   --       any unreachable subprograms that get deleted.

   --  d.4  By default in case of an error during C generation, the .c or .h
   --       file is deleted. This flag keeps the C file.

   --  d.5  By default a subprogram imported generates a subprogram profile.
   --       This debug flag disables this generation when generating C code,
   --       assuming a proper #include will be used instead.

   --  d.6  By default the C back-end avoids declaring types that are not
   --       referenced by the generated C code. This debug flag restores the
   --       output of all the types.

   --  d.7  Indicates (to gnat2scil) that CodePeer is being invoked as a
   --       prover by the SPARK tools and that therefore gnat2scil should
   --       avoid SCIL generation strategies which can introduce soundness
   --       issues (e.g., assuming that a low bound of an array parameter
   --       of an unconstrained subtype belongs to the index subtype).

   --  d.9  Enable build-in-place for function calls returning some nonlimited
   --       types.

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

   --  di  Normally GNATBIND calls Read_Ali with Ignore_Errors set to False,
   --      since the binder really needs correct version ALI files to do its
   --      job. This debug flag causes Ignore_Errors mode to be set for the
   --      binder (and is particularly useful for testing ignore errors mode).

   --  dn  List details of manipulation of Num_Pred values during execution of
   --      the algorithm used to determine a correct order of elaboration. This
   --      is useful in diagnosing any problems in its behavior.

   --  do  Use older elaboration order preference. The new preference rules
   --      prefer specs with no bodies to specs with bodies, and between two
   --      specs with bodies, prefers the one whose body is closer to being
   --      able to be elaborated. This is a clear improvement, but we provide
   --      this debug flag in case of regressions. Note: -do is even older
   --      than -dp.

   --  dp  Use old elaboration order preference. The new preference rules
   --      elaborate all units within a strongly connected component together,
   --      with no other units in between. In particular, if a spec/body pair
   --      can be elaborated together, it will be. In the new order, the binder
   --      behaves as if every pragma Elaborate_All that would be legal is
   --      present, even if it does not appear in the source code.

   --  du  List unit name and file name for each unit as it is read in

   --  dv  Verbose debugging printouts

   --  dx  Force the binder to read (and then ignore) the xref information
   --      in ali files (used to check that read circuit is working OK).

   --  d_a  GNATBIND ignores the effects of pragma Elaborate_All in the case of
   --       elaboration order and treats the associated dependency as a regular
   --       with edge.

   --  d_b  GNATBIND ignores the effects of pragma Elaborate_Body in the case
   --       of elaboration order and treats the spec and body as decoupled.

   --  d_e  GNATBIND ignores the effects of pragma Elaborate in the case of
   --       elaboration order and no longer creates an implicit dependency on
   --       the body of the argument.

   --  d_t  GNATBIND output trace information of cycle-detection activities to
   --       standard output.

   --  d_A  GNATBIND output the contents of all ALI invocation-related tables
   --       in textual format to standard output.

   --  d_C  GNATBIND diagnoses all unique cycles within the bind, rather than
   --       just the most important one.

   --  d_I  GNATBIND outputs the contents of the invocation graph in textual
   --       format to standard output.

   --  d_L  GNATBIND outputs the contents of the library graph in textual
   --       format to standard output.

   --  d_P  GNATBIND outputs the cycle paths to standard output

   --  d_S  GNATBIND outputs trace information concerning the status of its
   --       various phases to standard output.

   --  d_T  GNATBIND outputs trace information of elaboration order detection
   --       activities to standard output.

   --  d_V  GNATBIND validates the invocation graph, library graph along with
   --       its cycles, and the elaboration order.

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
   --      files or project path files. This debug switch is equivalent to
   --      the standard switch --keep-temp-files. We retain the debug switch
   --      for back compatibility with past usage.

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

   --  dm  Display the maximum number of simultaneous compilations.

   --  dn  Do not delete temporary files created by gprbuild at the end
   --      of execution, such as temporary config pragma files, mapping
   --      files or project path files. This debug switch is equivalent to
   --      the standard switch --keep-temp-files. We retain the debug switch
   --      for back compatibility with past usage.

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
            when '1' =>
               Debug_Flag_1 := Val;
            when '2' =>
               Debug_Flag_2 := Val;
            when '3' =>
               Debug_Flag_3 := Val;
            when '4' =>
               Debug_Flag_4 := Val;
            when '5' =>
               Debug_Flag_5 := Val;
            when '6' =>
               Debug_Flag_6 := Val;
            when '7' =>
               Debug_Flag_7 := Val;
            when '8' =>
               Debug_Flag_8 := Val;
            when '9' =>
               Debug_Flag_9 := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' =>
               Debug_Flag_AA := Val;
            when 'B' =>
               Debug_Flag_BB := Val;
            when 'C' =>
               Debug_Flag_CC := Val;
            when 'D' =>
               Debug_Flag_DD := Val;
            when 'E' =>
               Debug_Flag_EE := Val;
            when 'F' =>
               Debug_Flag_FF := Val;
            when 'G' =>
               Debug_Flag_GG := Val;
            when 'H' =>
               Debug_Flag_HH := Val;
            when 'I' =>
               Debug_Flag_II := Val;
            when 'J' =>
               Debug_Flag_JJ := Val;
            when 'K' =>
               Debug_Flag_KK := Val;
            when 'L' =>
               Debug_Flag_LL := Val;
            when 'M' =>
               Debug_Flag_MM := Val;
            when 'N' =>
               Debug_Flag_NN := Val;
            when 'O' =>
               Debug_Flag_OO := Val;
            when 'P' =>
               Debug_Flag_PP := Val;
            when 'Q' =>
               Debug_Flag_QQ := Val;
            when 'R' =>
               Debug_Flag_RR := Val;
            when 'S' =>
               Debug_Flag_SS := Val;
            when 'T' =>
               Debug_Flag_TT := Val;
            when 'U' =>
               Debug_Flag_UU := Val;
            when 'V' =>
               Debug_Flag_VV := Val;
            when 'W' =>
               Debug_Flag_WW := Val;
            when 'X' =>
               Debug_Flag_XX := Val;
            when 'Y' =>
               Debug_Flag_YY := Val;
            when 'Z' =>
               Debug_Flag_ZZ := Val;
         end case;

      else
         case LLet (C) is
            when 'a' =>
               Debug_Flag_A := Val;
            when 'b' =>
               Debug_Flag_B := Val;
            when 'c' =>
               Debug_Flag_C := Val;
            when 'd' =>
               Debug_Flag_D := Val;
            when 'e' =>
               Debug_Flag_E := Val;
            when 'f' =>
               Debug_Flag_F := Val;
            when 'g' =>
               Debug_Flag_G := Val;
            when 'h' =>
               Debug_Flag_H := Val;
            when 'i' =>
               Debug_Flag_I := Val;
            when 'j' =>
               Debug_Flag_J := Val;
            when 'k' =>
               Debug_Flag_K := Val;
            when 'l' =>
               Debug_Flag_L := Val;
            when 'm' =>
               Debug_Flag_M := Val;
            when 'n' =>
               Debug_Flag_N := Val;
            when 'o' =>
               Debug_Flag_O := Val;
            when 'p' =>
               Debug_Flag_P := Val;
            when 'q' =>
               Debug_Flag_Q := Val;
            when 'r' =>
               Debug_Flag_R := Val;
            when 's' =>
               Debug_Flag_S := Val;
            when 't' =>
               Debug_Flag_T := Val;
            when 'u' =>
               Debug_Flag_U := Val;
            when 'v' =>
               Debug_Flag_V := Val;
            when 'w' =>
               Debug_Flag_W := Val;
            when 'x' =>
               Debug_Flag_X := Val;
            when 'y' =>
               Debug_Flag_Y := Val;
            when 'z' =>
               Debug_Flag_Z := Val;
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
            when '1' =>
               Debug_Flag_Dot_1 := Val;
            when '2' =>
               Debug_Flag_Dot_2 := Val;
            when '3' =>
               Debug_Flag_Dot_3 := Val;
            when '4' =>
               Debug_Flag_Dot_4 := Val;
            when '5' =>
               Debug_Flag_Dot_5 := Val;
            when '6' =>
               Debug_Flag_Dot_6 := Val;
            when '7' =>
               Debug_Flag_Dot_7 := Val;
            when '8' =>
               Debug_Flag_Dot_8 := Val;
            when '9' =>
               Debug_Flag_Dot_9 := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' =>
               Debug_Flag_Dot_AA := Val;
            when 'B' =>
               Debug_Flag_Dot_BB := Val;
            when 'C' =>
               Debug_Flag_Dot_CC := Val;
            when 'D' =>
               Debug_Flag_Dot_DD := Val;
            when 'E' =>
               Debug_Flag_Dot_EE := Val;
            when 'F' =>
               Debug_Flag_Dot_FF := Val;
            when 'G' =>
               Debug_Flag_Dot_GG := Val;
            when 'H' =>
               Debug_Flag_Dot_HH := Val;
            when 'I' =>
               Debug_Flag_Dot_II := Val;
            when 'J' =>
               Debug_Flag_Dot_JJ := Val;
            when 'K' =>
               Debug_Flag_Dot_KK := Val;
            when 'L' =>
               Debug_Flag_Dot_LL := Val;
            when 'M' =>
               Debug_Flag_Dot_MM := Val;
            when 'N' =>
               Debug_Flag_Dot_NN := Val;
            when 'O' =>
               Debug_Flag_Dot_OO := Val;
            when 'P' =>
               Debug_Flag_Dot_PP := Val;
            when 'Q' =>
               Debug_Flag_Dot_QQ := Val;
            when 'R' =>
               Debug_Flag_Dot_RR := Val;
            when 'S' =>
               Debug_Flag_Dot_SS := Val;
            when 'T' =>
               Debug_Flag_Dot_TT := Val;
            when 'U' =>
               Debug_Flag_Dot_UU := Val;
            when 'V' =>
               Debug_Flag_Dot_VV := Val;
            when 'W' =>
               Debug_Flag_Dot_WW := Val;
            when 'X' =>
               Debug_Flag_Dot_XX := Val;
            when 'Y' =>
               Debug_Flag_Dot_YY := Val;
            when 'Z' =>
               Debug_Flag_Dot_ZZ := Val;
         end case;

      else
         case LLet (C) is
            when 'a' =>
               Debug_Flag_Dot_A := Val;
            when 'b' =>
               Debug_Flag_Dot_B := Val;
            when 'c' =>
               Debug_Flag_Dot_C := Val;
            when 'd' =>
               Debug_Flag_Dot_D := Val;
            when 'e' =>
               Debug_Flag_Dot_E := Val;
            when 'f' =>
               Debug_Flag_Dot_F := Val;
            when 'g' =>
               Debug_Flag_Dot_G := Val;
            when 'h' =>
               Debug_Flag_Dot_H := Val;
            when 'i' =>
               Debug_Flag_Dot_I := Val;
            when 'j' =>
               Debug_Flag_Dot_J := Val;
            when 'k' =>
               Debug_Flag_Dot_K := Val;
            when 'l' =>
               Debug_Flag_Dot_L := Val;
            when 'm' =>
               Debug_Flag_Dot_M := Val;
            when 'n' =>
               Debug_Flag_Dot_N := Val;
            when 'o' =>
               Debug_Flag_Dot_O := Val;
            when 'p' =>
               Debug_Flag_Dot_P := Val;
            when 'q' =>
               Debug_Flag_Dot_Q := Val;
            when 'r' =>
               Debug_Flag_Dot_R := Val;
            when 's' =>
               Debug_Flag_Dot_S := Val;
            when 't' =>
               Debug_Flag_Dot_T := Val;
            when 'u' =>
               Debug_Flag_Dot_U := Val;
            when 'v' =>
               Debug_Flag_Dot_V := Val;
            when 'w' =>
               Debug_Flag_Dot_W := Val;
            when 'x' =>
               Debug_Flag_Dot_X := Val;
            when 'y' =>
               Debug_Flag_Dot_Y := Val;
            when 'z' =>
               Debug_Flag_Dot_Z := Val;
         end case;
      end if;
   end Set_Dotted_Debug_Flag;

   --------------------------------
   -- Set_Underscored_Debug_Flag --
   --------------------------------

   procedure Set_Underscored_Debug_Flag
     (C   : Character;
      Val : Boolean := True)
   is
      subtype Dig  is Character range '1' .. '9';
      subtype LLet is Character range 'a' .. 'z';
      subtype ULet is Character range 'A' .. 'Z';

   begin
      if C in Dig then
         case Dig (C) is
            when '1' =>
               Debug_Flag_Underscore_1 := Val;
            when '2' =>
               Debug_Flag_Underscore_2 := Val;
            when '3' =>
               Debug_Flag_Underscore_3 := Val;
            when '4' =>
               Debug_Flag_Underscore_4 := Val;
            when '5' =>
               Debug_Flag_Underscore_5 := Val;
            when '6' =>
               Debug_Flag_Underscore_6 := Val;
            when '7' =>
               Debug_Flag_Underscore_7 := Val;
            when '8' =>
               Debug_Flag_Underscore_8 := Val;
            when '9' =>
               Debug_Flag_Underscore_9 := Val;
         end case;

      elsif C in ULet then
         case ULet (C) is
            when 'A' =>
               Debug_Flag_Underscore_AA := Val;
            when 'B' =>
               Debug_Flag_Underscore_BB := Val;
            when 'C' =>
               Debug_Flag_Underscore_CC := Val;
            when 'D' =>
               Debug_Flag_Underscore_DD := Val;
            when 'E' =>
               Debug_Flag_Underscore_EE := Val;
            when 'F' =>
               Debug_Flag_Underscore_FF := Val;
            when 'G' =>
               Debug_Flag_Underscore_GG := Val;
            when 'H' =>
               Debug_Flag_Underscore_HH := Val;
            when 'I' =>
               Debug_Flag_Underscore_II := Val;
            when 'J' =>
               Debug_Flag_Underscore_JJ := Val;
            when 'K' =>
               Debug_Flag_Underscore_KK := Val;
            when 'L' =>
               Debug_Flag_Underscore_LL := Val;
            when 'M' =>
               Debug_Flag_Underscore_MM := Val;
            when 'N' =>
               Debug_Flag_Underscore_NN := Val;
            when 'O' =>
               Debug_Flag_Underscore_OO := Val;
            when 'P' =>
               Debug_Flag_Underscore_PP := Val;
            when 'Q' =>
               Debug_Flag_Underscore_QQ := Val;
            when 'R' =>
               Debug_Flag_Underscore_RR := Val;
            when 'S' =>
               Debug_Flag_Underscore_SS := Val;
            when 'T' =>
               Debug_Flag_Underscore_TT := Val;
            when 'U' =>
               Debug_Flag_Underscore_UU := Val;
            when 'V' =>
               Debug_Flag_Underscore_VV := Val;
            when 'W' =>
               Debug_Flag_Underscore_WW := Val;
            when 'X' =>
               Debug_Flag_Underscore_XX := Val;
            when 'Y' =>
               Debug_Flag_Underscore_YY := Val;
            when 'Z' =>
               Debug_Flag_Underscore_ZZ := Val;
         end case;

      else
         case LLet (C) is
            when 'a' =>
               Debug_Flag_Underscore_A := Val;
            when 'b' =>
               Debug_Flag_Underscore_B := Val;
            when 'c' =>
               Debug_Flag_Underscore_C := Val;
            when 'd' =>
               Debug_Flag_Underscore_D := Val;
            when 'e' =>
               Debug_Flag_Underscore_E := Val;
            when 'f' =>
               Debug_Flag_Underscore_F := Val;
            when 'g' =>
               Debug_Flag_Underscore_G := Val;
            when 'h' =>
               Debug_Flag_Underscore_H := Val;
            when 'i' =>
               Debug_Flag_Underscore_I := Val;
            when 'j' =>
               Debug_Flag_Underscore_J := Val;
            when 'k' =>
               Debug_Flag_Underscore_K := Val;
            when 'l' =>
               Debug_Flag_Underscore_L := Val;
            when 'm' =>
               Debug_Flag_Underscore_M := Val;
            when 'n' =>
               Debug_Flag_Underscore_N := Val;
            when 'o' =>
               Debug_Flag_Underscore_O := Val;
            when 'p' =>
               Debug_Flag_Underscore_P := Val;
            when 'q' =>
               Debug_Flag_Underscore_Q := Val;
            when 'r' =>
               Debug_Flag_Underscore_R := Val;
            when 's' =>
               Debug_Flag_Underscore_S := Val;
            when 't' =>
               Debug_Flag_Underscore_T := Val;
            when 'u' =>
               Debug_Flag_Underscore_U := Val;
            when 'v' =>
               Debug_Flag_Underscore_V := Val;
            when 'w' =>
               Debug_Flag_Underscore_W := Val;
            when 'x' =>
               Debug_Flag_Underscore_X := Val;
            when 'y' =>
               Debug_Flag_Underscore_Y := Val;
            when 'z' =>
               Debug_Flag_Underscore_Z := Val;
         end case;
      end if;
   end Set_Underscored_Debug_Flag;

end Debug;
