------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . X R E F                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains for collecting and outputting cross-reference
--  information.

with Einfo;       use Einfo;
with SPARK_Xrefs;

package Lib.Xref is

   -------------------------------------------------------
   -- Format of Cross-Reference Information in ALI File --
   -------------------------------------------------------

   --  Cross-reference sections follow the dependency section (D lines) in
   --  an ALI file, so that they need not be read by gnatbind, gnatmake etc.

   --  A cross reference section has a header of the form

   --     X  dependency-number  filename

   --        This header precedes xref information (entities/references from
   --        the unit), identified by dependency number and file name. The
   --        dependency number is the index into the generated D lines and
   --        is ones origin (e.g. 2 = reference to second generated D line).

   --        Note that the filename here will reflect the original name if
   --        a Source_Reference pragma was encountered (since all line number
   --        references will be with respect to the original file).

   --  The lines following the header look like

   --  line type col level entity renameref instref typeref overref ref ref

   --        line is the line number of the referenced entity. The name of
   --        the entity starts in column col. Columns are numbered from one,
   --        and if horizontal tab characters are present, the column number
   --        is computed assuming standard 1,9,17,.. tab stops. For example,
   --        if the entity is the first token on the line, and is preceded
   --        by space-HT-space, then the column would be column 10.

   --        type is a single letter identifying the type of the entity. See
   --        next section (Cross-Reference Entity Identifiers) for a full list
   --        of the characters used).

   --        col is the column number of the referenced entity

   --        level is a single character that separates the col and entity
   --        fields. It is an asterisk (*) for a top level library entity that
   --        is publicly visible, as well for an entity declared in the visible
   --        part of a generic package, the plus sign (+) for a C/C++ static
   --        entity, and space otherwise.

   --        entity is the name of the referenced entity, with casing in
   --        the canonical casing for the source file where it is defined.

   --        renameref provides information on renaming. If the entity is
   --        a package, object or overloadable entity which is declared by
   --        a renaming declaration, and the renaming refers to an entity
   --        with a simple identifier or expanded name, then renameref has
   --        the form:

   --            =line:col

   --        Here line:col give the reference to the identifier that appears
   --        in the renaming declaration. Note that we never need a file entry,
   --        since this identifier is always in the current file in which the
   --        entity is declared. Currently, renameref appears only for the
   --        simple renaming case. If the renaming reference is a complex
   --        expressions, then renameref is omitted. Here line/col give
   --        line/column as defined above.

   --        instref is only present for package and subprogram instances. The
   --        information in instref is the location of the point of declaration
   --        of the generic parent unit. This part has the form:

   --            [file|line]

   --        without column information, on the reasonable assumption that
   --        there is only one unit per line (the same assumption is made in
   --        references to entities declared within instances, see below).

   --        typeref is the reference for a related type. This part is
   --        optional. It is present for the following cases:

   --          derived types (points to the parent type)   LR=<>
   --          access types (points to designated type)    LR=()
   --          array types (points to component type)      LR=()
   --          subtypes (points to ancestor type)          LR={}
   --          functions (points to result type)           LR={}
   --          enumeration literals (points to enum type)  LR={}
   --          objects and components (points to type)     LR={}

   --          For a type that implements multiple interfaces, there is an
   --          entry of the form  LR=<> for each of the interfaces appearing
   --          in the type declaration. In the data structures of ali.ads,
   --          the type that the entity extends (or the first interface if
   --          there is no such type) is stored in Xref_Entity_Record.Tref*,
   --          additional interfaces are stored in the list of references
   --          with a special type of Interface_Reference.

   --          For an array type, there is an entry of the form LR=<> for each
   --          of the index types appearing in the type declaration. The index
   --          types follow the entry for the component type. In the data
   --          structures of ali.ads, however, the list of index types are
   --          output in the list of references with a special Rtype set to
   --          Array_Index_Reference.

   --          In the above list LR shows the brackets used in the output which
   --          has one of the two following forms:

   --            L file | line type col R      user entity
   --            L name-in-lower-case R        standard entity

   --          For the form for a user entity, file is the dependency number
   --          of the file containing the declaration of the related type.
   --          This number and the following vertical bar are omitted if the
   --          relevant type is defined in the same file as the current entity.
   --          The line, type, col are defined as previously described, and
   --          specify the location of the relevant type declaration in the
   --          referenced file. For the standard entity form, the name between
   --          the brackets is the normal name of the entity in lower case.

   --        overref is present for overriding operations (procedures and
   --        functions), and provides information on the operation that it
   --        overrides. This information has the format:

   --        '<' file | line 'o' col '>'

   --           file is the dependency number of the file containing the
   --           declaration of the overridden operation. It and the following
   --           vertical bar are omitted if the file is the same as that of
   --           the overriding operation.

   --     There may be zero or more ref entries on each line

   --        file | line type col [...]

   --           file is the dependency number of the file with the reference.
   --           It and the following vertical bar are omitted if the file is
   --           the same as the previous ref, and the refs for the current
   --           file are first (and do not need a bar).

   --           line is the line number of the reference

   --           col is the column number of the reference, as defined above

   --           type is one of
   --              b = body entity
   --              c = completion of private or incomplete type
   --              d = discriminant of type
   --              D = object definition
   --              e = end of spec
   --              E = first private entity
   --              H = abstract type
   --              i = implicit reference
   --              k = implicit reference to parent unit in child unit
   --              l = label on END line
   --              m = modification
   --              o = own variable reference (SPARK only)
   --              p = primitive operation
   --              P = overriding primitive operation
   --              r = reference
   --              R = subprogram reference in dispatching call
   --              s = subprogram reference in a static call
   --              t = end of body
   --              w = WITH line
   --              x = type extension
   --              z = generic formal parameter
   --              > = subprogram IN parameter
   --              = = subprogram IN OUT parameter
   --              < = subprogram OUT parameter
   --              ^ = subprogram ACCESS parameter

   --           b is used for spec entities that are repeated in a body,
   --           including the unit (subprogram, package, task, protected body,
   --           protected entry) name itself, and in the case of a subprogram,
   --           the formals. This letter is also used for the occurrence of
   --           entry names in accept statements. Such entities are not
   --           considered to be definitions for cross-referencing purposes,
   --           but rather are considered to be references to the corresponding
   --           spec entities, marked with this special type.

   --           c is similar to b but is used to mark the completion of a
   --           private or incomplete type. As with b, the completion is not
   --           regarded as a separate definition, but rather a reference to
   --           the initial declaration, marked with this special type.

   --           d is used to identify a discriminant of a type. If this is
   --           an incomplete or private type with discriminants, the entry
   --           denotes the occurrence of the discriminant in the partial view
   --           which is also the point of definition of the discriminant. The
   --           occurrence of the same discriminant in the full view is a
   --           regular reference to it.

   --           e is used to identify the end of a construct in the following
   --           cases:

   --             Block Statement        end [block_IDENTIFIER];
   --             Loop Statement         end loop [loop_IDENTIFIER];
   --             Package Specification  end [[PARENT_UNIT_NAME .] IDENTIFIER];
   --             Task Definition        end [task_IDENTIFIER];
   --             Protected Definition   end [protected_IDENTIFIER];
   --             Record Definition      end record;
   --             Enumeration Definition );

   --           Note that 'e' entries are special in that they appear even
   --           in referencing units (normally xref entries appear only for
   --           references in the extended main source unit (see Lib) to which
   --           the ali applies. But 'e' entries are really structural and
   --           simply indicate where packages end. This information can be
   --           used to reconstruct scope information for any entities
   --           referenced from within the package. The line/column values
   --           for these entries point to the semicolon ending the construct.

   --           i is used to identify a reference to the entity in a generic
   --           actual or in a default in a call. The node that denotes the
   --           entity does not come from source, but it has the Sloc of the
   --           source node that generates the implicit reference, and it is
   --           useful to record this one.

   --           k is another non-standard reference type, used to record a
   --           reference from a child unit to its parent. For various cross-
   --           referencing tools, we need a pointer from the xref entries for
   --           the child to the parent. This is the opposite way round from
   --           normal xref entries, since the reference is *from* the child
   --           unit *to* the parent unit, yet appears in the xref entries for
   --           the child. Consider this example:
   --
   --             package q is
   --             end;
   --             package q.r is
   --             end q.r;
   --
   --           The ali file for q-r.ads has these entries
   --
   --             D q.ads
   --             D q-r.ads
   --             D system.ads
   --             X 1 q.ads
   --             1K9*q 2e4 2|1r9 2r5
   --             X 2 q-r.ads
   --             1K11*r 1|1k9 2|2l7 2e8
   --
   --           Here the 2|1r9 entry appearing in the section for the parent
   --           is the normal reference from the child to the parent. The 1k9
   --           entry in the section for the child duplicates this information
   --           but appears in the child rather than the parent.

   --           l is used to identify the occurrence in the source of the name
   --           on an end line. This is just a syntactic reference which can be
   --           ignored for semantic purposes (e.g. a call graph construction).
   --           Again, in the case of an accept there can be multiple l lines.

   --           o is used for variables referenced from a SPARK 'own'
   --           definition. In the SPARK language, it is allowed to use a
   --           variable before its actual declaration.

   --           p is used to mark a primitive operation of the given entity.
   --           For example, if we have a type Tx, and a primitive operation
   --           Pq of this type, then an entry in the list of references to
   --           Tx will point to the declaration of Pq. Note that this entry
   --           type is unusual because it an implicit rather than explicit,
   --           and the name of the reference does not match the name of the
   --           entity for which a reference is generated. These entries are
   --           generated only for entities declared in the extended main
   --           source unit (main unit itself, its separate spec (if any).
   --           and all subunits (considered recursively).

   --           If the primitive operation overrides an inherited primitive
   --           operation of the parent type, the letter 'P' is used in the
   --           corresponding entry.

   --           R is used to mark a dispatching call. The reference is to
   --           the specification of the primitive operation of the root
   --           type when the call has a controlling argument in its class.

   --           s is used to mark a static subprogram call. The reference is
   --           to the specification of the subprogram being called.

   --           t is similar to e. It identifies the end of a corresponding
   --           body (such a reference always links up with a b reference)

   --             Subprogram Body        end [DESIGNATOR];
   --             Package Body           end [[PARENT_UNIT_NAME .] IDENTIFIER];
   --             Task Body              end [task_IDENTIFIER];
   --             Entry Body             end [entry_IDENTIFIER];
   --             Protected Body         end [protected_IDENTIFIER]
   --             Accept Statement       end [entry_IDENTIFIER]];

   --           Note that in the case of accept statements, there can
   --           be multiple b and t entries for the same entity.

   --           x is used to identify the reference as the entity from which a
   --           tagged type is extended. This allows immediate access to the
   --           parent of a tagged type.

   --           z is used on the cross-reference line for a generic unit,
   --           to mark the definition of a generic formal of the unit. This
   --           entry type is similar to 'k' and 'p' in that it is an implicit
   --           reference for an entity with a different name.

   --           The characters >, <. =, and ^ are used on the cross-reference
   --           line for a subprogram, to denote formal parameters and their
   --           modes. As with the 'z' and 'p' entries, each such entry is
   --           an implicit reference to an entity with a different name.

   --           [..] is used for generic instantiation references. These
   --           references are present only if the entity in question is
   --           a generic entity, and in that case the [..] contains the
   --           reference for the instantiation. In the case of nested
   --           instantiations, this can be nested [...[...[...]]] etc. The
   --           reference is of the form [file|line] no column is present since
   --           it is assumed that only one instantiation appears on a single
   --           source line. Note that the appearance of file numbers in such
   --           references follows the normal rules (present only if needed,
   --           and resets the current file for subsequent references).

   --     Examples:

   --        44B5*Flag_Type{boolean} 5r23 6m45 3|9r35 11r56

   --           This line gives references for the publicly visible Boolean
   --           type Flag_Type declared on line 44, column 5. There are four
   --           references

   --              a reference on line 5, column 23 of the current file

   --              a modification on line 6, column 45 of the current file

   --              a reference on line 9, column 35 of unit number 3

   --              a reference on line 11, column 56 of unit number 3

   --        2U13 p3=2:35 5b13 8r4 12r13 12t15

   --           This line gives references for the non-publicly visible
   --           procedure p3 declared on line 2, column 13. This procedure
   --           renames the procedure whose identifier reference is at
   --           line 2 column 35. There are four references:

   --              the corresponding body entity at line 5, column 13,
   --              of the current file.

   --              a reference (e.g. a call) at line 8 column 4 of the
   --              current file.

   --              the END line of the body has an explicit reference to
   --              the name of the procedure at line 12, column 13.

   --              the body ends at line 12, column 15, just past this label

   --        16I9*My_Type<2|4I9> 18r8

   --           This line gives references for the publicly visible Integer
   --           derived type My_Type declared on line 16, column 9. It also
   --           gives references to the parent type declared in the unit
   --           number 2 on line 4, column 9. There is one reference:

   --              a reference (e.g. a variable declaration) at line 18 column
   --              4 of the current file.

   --        10I3*Genv{integer} 3|4I10[6|12]

   --           This line gives a reference for the entity Genv in a generic
   --           package. The reference in file 3, line 4, col 10, refers to an
   --           instance of the generic where the instantiation can be found in
   --           file 6 at line 12.

   --  Continuation lines are used if the reference list gets too long,
   --  a continuation line starts with a period, and then has references
   --  continuing from the previous line. The references are sorted first
   --  by unit, then by position in the source.

   --  Note on handling of generic entities. The cross-reference is oriented
   --  towards source references, so the entities in a generic instantiation
   --  are not considered distinct from the entities in the template. All
   --  definitions and references from generic instantiations are suppressed,
   --  since they will be generated from the template. Any references to
   --  entities in a generic instantiation from outside the instantiation
   --  are considered to be references to the original template entity.

   ----------------------------------------
   -- Cross-Reference Entity Identifiers --
   ----------------------------------------

   --  In the cross-reference section of the ali file, entity types are
   --  identified by a single letter, indicating the entity type. The following
   --  table indicates the letter. A space for an entry is used for entities
   --  that do not appear in the cross-reference table.

   --  For objects, the character * appears in this table. In the xref listing,
   --  this character is replaced by the lower case letter that corresponds to
   --  the type of the object. For example, if a variable is of a Float type,
   --  then, since the type is represented by an upper case F, the object would
   --  be represented by a lower case f.

   --  A special exception is the case of booleans, whose entities are normal
   --  E_Enumeration_Type or E_Enumeration_Subtype entities, but which appear
   --  as B/b in the xref lines, rather than E/e.

   --  For private types, the character + appears in the table. In this case
   --  the kind of the underlying type is used, if available, to determine the
   --  character to use in the xref listing. The listing will still include a
   --  '+' for a generic private type, for example, but will retain the '*' for
   --  an object or formal parameter of such a type.

   --  For subprograms, the characters 'U' and 'V' appear in the table,
   --  indicating procedures and functions. If the operation is abstract,
   --  these letters are replaced in the xref by 'x' and 'y' respectively.

   Xref_Entity_Letters : constant array (Entity_Kind) of Character :=
     (E_Abstract_State                             => '@',
      E_Access_Attribute_Type                      => 'P',
      E_Access_Protected_Subprogram_Type           => 'P',
      E_Access_Subprogram_Type                     => 'P',
      E_Access_Subtype                             => 'P',
      E_Access_Type                                => 'P',
      E_Allocator_Type                             => ' ',
      E_Anonymous_Access_Protected_Subprogram_Type => ' ',
      E_Anonymous_Access_Subprogram_Type           => ' ',
      E_Anonymous_Access_Type                      => ' ',
      E_Array_Subtype                              => 'A',
      E_Array_Type                                 => 'A',
      E_Block                                      => 'q',
      E_Class_Wide_Subtype                         => 'C',
      E_Class_Wide_Type                            => 'C',
      E_Component                                  => '*',
      E_Constant                                   => '*',
      E_Decimal_Fixed_Point_Subtype                => 'D',
      E_Decimal_Fixed_Point_Type                   => 'D',
      E_Discriminant                               => '*',
      E_Entry                                      => 'Y',
      E_Entry_Family                               => 'Y',
      E_Entry_Index_Parameter                      => '*',
      E_Enumeration_Literal                        => 'n',
      E_Enumeration_Subtype                        => 'E',  -- B for boolean
      E_Enumeration_Type                           => 'E',  -- B for boolean
      E_Exception                                  => 'X',
      E_Exception_Type                             => ' ',
      E_Floating_Point_Subtype                     => 'F',
      E_Floating_Point_Type                        => 'F',
      E_Function                                   => 'V',
      E_General_Access_Type                        => 'P',
      E_Generic_Function                           => 'v',
      E_Generic_In_Out_Parameter                   => '*',
      E_Generic_In_Parameter                       => '*',
      E_Generic_Package                            => 'k',
      E_Generic_Procedure                          => 'u',
      E_Label                                      => 'L',
      E_Limited_Private_Subtype                    => '+',
      E_Limited_Private_Type                       => '+',
      E_Loop                                       => 'l',
      E_Loop_Parameter                             => '*',
      E_In_Out_Parameter                           => '*',
      E_In_Parameter                               => '*',
      E_Incomplete_Subtype                         => '+',
      E_Incomplete_Type                            => '+',
      E_Modular_Integer_Subtype                    => 'M',
      E_Modular_Integer_Type                       => 'M',
      E_Named_Integer                              => 'N',
      E_Named_Real                                 => 'N',
      E_Operator                                   => 'V',
      E_Ordinary_Fixed_Point_Subtype               => 'O',
      E_Ordinary_Fixed_Point_Type                  => 'O',
      E_Out_Parameter                              => '*',
      E_Package                                    => 'K',
      E_Private_Subtype                            => '+',
      E_Private_Type                               => '+',
      E_Procedure                                  => 'U',
      E_Protected_Subtype                          => 'W',
      E_Protected_Type                             => 'W',
      E_Record_Subtype                             => 'R',
      E_Record_Subtype_With_Private                => 'R',
      E_Record_Type                                => 'R',
      E_Record_Type_With_Private                   => 'R',
      E_Return_Statement                           => ' ',
      E_Signed_Integer_Subtype                     => 'I',
      E_Signed_Integer_Type                        => 'I',
      E_String_Literal_Subtype                     => ' ',
      E_Subprogram_Type                            => ' ',
      E_Task_Subtype                               => 'T',
      E_Task_Type                                  => 'T',
      E_Variable                                   => '*',
      E_Void                                       => ' ',

      --  The following entities are not ones to which we gather the cross-
      --  references, since it does not make sense to do so (e.g. references
      --  to a package are to the spec, not the body). Indeed the occurrence of
      --  the body entity is considered to be a reference to the spec entity.

      E_Package_Body                               => ' ',
      E_Protected_Body                             => ' ',
      E_Protected_Object                           => ' ',
      E_Subprogram_Body                            => ' ',
      E_Task_Body                                  => ' ');

   --  The following table is for information purposes. It shows the use of
   --  each character appearing as an entity type.

   --  letter  lower case usage                UPPER CASE USAGE

   --    a     array object (except string)    array type (except string)
   --    b     Boolean object                  Boolean type
   --    c     class-wide object               class-wide type
   --    d     decimal fixed-point object      decimal fixed-point type
   --    e     non-Boolean enumeration object  non_Boolean enumeration type
   --    f     floating-point object           floating-point type
   --    g     C/C++ macro                     C/C++ fun-like macro
   --    h     Interface (Ada 2005)            Abstract type
   --    i     signed integer object           signed integer type
   --    j     C++ class object                C++ class
   --    k     generic package                 package
   --    l     label on loop                   label on statement
   --    m     modular integer object          modular integer type
   --    n     enumeration literal             named number
   --    o     ordinary fixed-point object     ordinary fixed-point type
   --    p     access object                   access type
   --    q     label on block                  C/C++ include file
   --    r     record object                   record type
   --    s     string object                   string type
   --    t     task object                     task type
   --    u     generic procedure               procedure
   --    v     generic function or operator    function or operator
   --    w     protected object                protected type
   --    x     abstract procedure              exception
   --    y     abstract function               entry or entry family
   --    z     generic formal parameter        (unused)

   ---------------------------------------------------
   -- Handling of Imported and Exported Subprograms --
   ---------------------------------------------------

   --  If a pragma Import or Interface applies to a subprogram, the pragma is
   --  the completion of the subprogram. This is noted in the ALI file by
   --  making the occurrence of the subprogram in the pragma into a body
   --  reference ('b') and by including the external name of the subprogram and
   --  its language, bracketed by '<' and '>' in that reference. For example:

   --     3U13*imported_proc 4b<c,there>21

   --  indicates that procedure imported_proc, declared at line 3, has a pragma
   --  Import at line 4, that its body is in C, and that the link name as given
   --  in the pragma is "there".

   --  If a pragma Export applies to a subprogram exported to a foreign
   --  language (ie. the pragma has convention different from Ada), then the
   --  pragma is annotated in the ALI file by making the occurrence of the
   --  subprogram in the pragma into an implicit reference ('i') and by
   --  including the external name of the subprogram and its language,
   --  bracketed by '<' and '>' in that reference. For example:

   --     3U13*exported_proc 4i<c,here>21

   --  indicates that procedure exported_proc, declared at line 3, has a pragma
   --  Export at line 4, that its body is exported to C, and that the link name
   --  as given in the pragma is "here".

   -------------------------
   -- Deferred_References --
   -------------------------

   --  Normally we generate references as we go along, but as discussed in
   --  Sem_Util.Is_LHS, and Sem_Ch8.Find_Direct_Name/Find_Selected_Component,
   --  we have one case where that is tricky, which is when we have something
   --  like X.A := 3, where we don't know until we know the type of X whether
   --  this is a reference (if X is an access type, so what we really have is
   --  X.all.A := 3) or a modification, where X is not an access type.

   --  What we do in such cases is to gather nodes, where we would have liked
   --  to call Generate_Reference but we couldn't because we didn't know enough
   --  into this table, Then we deal with generating references later on when
   --  we have sufficient information to do it right.

   type Deferred_Reference_Entry is record
      E : Entity_Id;
      N : Node_Id;
   end record;
   --  One entry, E, N are as required for Generate_Reference call

   package Deferred_References is new Table.Table (
     Table_Component_Type => Deferred_Reference_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 512,
     Table_Increment      => 200,
     Table_Name           => "Name_Deferred_References");

   procedure Process_Deferred_References;
   --  This procedure is called from Frontend to process these table entries.
   --  It is also called from Sem_Warn.

   function Has_Deferred_Reference (Ent : Entity_Id) return Boolean;
   --  Determine whether arbitrary entity Ent has a pending reference in order
   --  to suppress premature warnings about useless assignments. See comments
   --  in Analyze_Assignment in sem_ch5.adb.

   -----------------------------
   -- SPARK Xrefs Information --
   -----------------------------

   --  This package defines procedures for collecting SPARK cross-reference
   --  information and printing in ALI files.

   package SPARK_Specific is

      function Enclosing_Subprogram_Or_Library_Package
        (N : Node_Id) return Entity_Id;
      --  Return the closest enclosing subprogram or library-level package.
      --  This ensures that GNATprove can distinguish local variables from
      --  global variables.
      --
      --  ??? This routine should only be used for processing related to
      --  cross-references, where it might return wrong result but must avoid
      --  crashes on ill-formed source code. It is wrong to use it where exact
      --  result is needed.

      procedure Generate_Dereference
        (N   : Node_Id;
         Typ : Character := 'r');
      --  This procedure is called to record a dereference. N is the location
      --  of the dereference.

      generic
         with procedure Process
           (Index : Int;
            Xref  : SPARK_Xrefs.SPARK_Xref_Record);
      procedure Iterate_SPARK_Xrefs;
      --  Call Process on cross-references relevant to the SPARK backend with
      --  parameter Xref holding the relevant subset of the xref entry and
      --  Index holding the position in the original tables with references
      --  (if positive) or dereferences (if negative).

   end SPARK_Specific;

   -----------------
   -- Subprograms --
   -----------------

   procedure Generate_Definition (E : Entity_Id);
   --  Records the definition of an entity

   procedure Generate_Operator_Reference
     (N : Node_Id;
      T : Entity_Id);
   --  Node N is an operator node, whose entity has been set. If this entity
   --  is a user defined operator (i.e. an operator not defined in package
   --  Standard), then a reference to the operator is recorded at node N.
   --  T is the operand type of the operator. A reference to the operator is an
   --  implicit reference to the type, and that needs to be recorded to avoid
   --  spurious warnings on unused entities, when the operator is a renaming of
   --  a predefined operator.

   procedure Generate_Reference
     (E       : Entity_Id;
      N       : Node_Id;
      Typ     : Character := 'r';
      Set_Ref : Boolean   := True;
      Force   : Boolean   := False);
   --  This procedure is called to record a reference. N is the location of the
   --  reference and E is the referenced entity. Typ is one of:
   --
   --    a character already described in the description of ref entries above
   --    ' ' for dummy reference (see below)
   --
   --  Note: all references to incomplete or private types are to the original
   --  (incomplete or private type) declaration. The full declaration is
   --  treated as a reference with type 'c'.
   --
   --  Note: all references to packages or subprograms are to the entity for
   --  the spec. The entity in the body is treated as a reference with type
   --  'b'. Similar handling for references to subprogram formals.
   --
   --  The call has no effect if N is not in the extended main source unit.
   --  This check is omitted for type 'e' references (where it is useful to
   --  have structural scoping information for other than the main source),
   --  and for 'p' (since we want to pick up inherited primitive operations
   --  that are defined in other packages).
   --
   --  The call also has no effect if any of the following conditions hold:
   --
   --    cross-reference collection is disabled
   --    entity does not come from source (and Force is False)
   --    reference does not come from source (and Force is False)
   --    the entity is not one for which xrefs are appropriate
   --    the type letter is blank
   --    the node N is not an identifier, defining identifier, or expanded name
   --    the type is 'p' and the entity is not in the extended main source
   --
   --  If all these conditions are met, then the Is_Referenced flag of E is set
   --  (unless Set_Ref is False) and a cross-reference entry is recorded for
   --  later output when Output_References is called.
   --
   --  Note: the dummy space entry is for the convenience of some callers,
   --  who find it easier to pass a space to suppress the entry than to do
   --  a specific test. The call has no effect if the type is a space.
   --
   --  The parameter Set_Ref is normally True, and indicates that in addition
   --  to generating a cross-reference, the Referenced flag of the specified
   --  entity should be set. If this parameter is False, then setting of the
   --  Referenced flag is inhibited.
   --
   --  The parameter Force is set to True to force a reference to be generated
   --  even if Comes_From_Source is false. This is used for certain implicit
   --  references, and also for end label references.

   procedure Generate_Reference_To_Formals (E : Entity_Id);
   --  Add a reference to the definition of each formal on the line for
   --  a subprogram or an access_to_subprogram type.

   procedure Generate_Reference_To_Generic_Formals (E : Entity_Id);
   --  Add a reference to the definition of each generic formal on the line
   --  for a generic unit.

   procedure Output_References;
   --  Output references to the current ali file

   procedure Initialize;
   --  Initialize internal tables

end Lib.Xref;
