------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . X R E F                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--          Copyright (C) 1998-2001, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains for collecting and outputting cross-reference
--  information.

with Einfo; use Einfo;
with Types; use Types;

package Lib.Xref is

   -------------------------------------------------------
   -- Format of Cross-Reference Information in ALI File --
   -------------------------------------------------------

   --  Cross-reference sections follow the dependency section (D lines) in
   --  an ALI file, so that they need not be read by gnatbind, gnatmake etc.
   --
   --  A cross reference section has a header of the form
   --
   --     X  dependency-number  filename
   --
   --        This header precedes xref information (entities/references from
   --        the unit, identified by dependency number and file name. The
   --        dependency number is the index into the generated D lines and
   --        is ones origin (i.e. 2 = reference to second generated D line).
   --
   --        Note that the filename here will reflect the original name if
   --        a Source_Reference pragma was encountered (since all line number
   --        references will be with respect to the original file).
   --
   --  The lines following the header look like
   --
   --     line type col level entity renameref typeref ref  ref  ref
   --
   --        line is the line number of the referenced entity. It starts
   --        in column one.
   --
   --        type is a single letter identifying the type of the entity.
   --        See next section (Cross-Reference Entity Identifiers) for a
   --        full list of the characters used).
   --
   --        col is the column number of the referenced entity
   --
   --        level is a single character that separates the col and
   --        entity fields. It is an asterisk for a top level library
   --        entity that is publicly visible, and space otherwise.
   --
   --        entity is the name of the referenced entity, with casing in
   --        the canical casing for the source file where it is defined.

   --        renameref provides information on renaming. If the entity is
   --        a package, object or overloadable entity which is declared by
   --        a renaming declaration, and the renaming refers to an entity
   --        with a simple identifier or expanded name, then renameref has
   --        the form:
   --
   --            =line:col
   --
   --        Here line:col give the reference to the identifier that
   --        appears in the renaming declaration. Note that we never need
   --        a file entry, since this identifier is always in the current
   --        file in which the entity is declared. Currently, renameref
   --        appears only for the simple renaming case. If the renaming
   --        reference is a complex expressions, then renameref is omitted.
   --
   --        typeref is the reference for a related type. This part is
   --        optional. It is present for the following cases:
   --
   --          derived types (points to the parent type)   LR=<>
   --          access types (points to designated type)    LR=()
   --          subtypes (points to ancestor type)          LR={}
   --          functions (points to result type)           LR={}
   --          enumeration literals (points to enum type)  LR={}
   --          objects and components (points to type)     LR={}
   --
   --          In the above list LR shows the brackets used in the output,
   --          which has one of the two following forms:
   --
   --            L file | line type col R      user entity
   --            L name-in-lower-case   R      standard entity
   --
   --          For the form for a user entity, file is the dependency number
   --          of the file containing the declaration of the related type.
   --          This number and the following vertical bar are omitted if the
   --          relevant type is defined in the same file as the current entity.
   --          The line, type, col are defined as previously described, and
   --          specify the location of the relevant type declaration in the
   --          referenced file. For the standard entity form, the name between
   --          the brackets is the normal name of the entity in lower case.
   --
   --     There may be zero or more ref entries on each line
   --
   --        file | line type col [...]
   --
   --           file is the dependency number of the file with the reference.
   --           It and the following vertical bar are omitted if the file is
   --           the same as the previous ref, and the refs for the current
   --           file are first (and do not need a bar).
   --
   --           type is one of
   --              r = reference
   --              m = modification
   --              b = body entity
   --              c = completion of private or incomplete type
   --              x = type extension
   --              i = implicit reference
   --              e = end of spec
   --              t = end of body
   --
   --           b is used for spec entities that are repeated in a body,
   --           including the unit (subprogram, package, task, protected
   --           body, protected entry) name itself, and in the case of a
   --           subprogram, the formals. This letter is also used for the
   --           occurrence of entry names in accept statements. Such entities
   --           are not considered to be definitions for cross-referencing
   --           purposes, but rather are considered to be references to the
   --           corresponding spec entities, marked with this special type.
   --
   --           c is similarly used to mark the completion of a private or
   --           incomplete type. Again, the completion is not regarded as
   --           a separate definition, but rather a reference to the initial
   --           declaration, marked with this special type.
   --
   --           x is used to identify the reference as the entity from which
   --           a tagged type is extended. This allows immediate access to
   --           the parent of a tagged type.
   --
   --           i is used to identify a reference to the entity in a generic
   --           actual or in a default in a call. The node that denotes the
   --           entity does not come from source, but it has the Sloc of the
   --           source node that generates the implicit reference, and it is
   --           useful to record this one.
   --
   --           e is used to identify the end of a construct in the following
   --           cases:
   --
   --             Block Statement        end [block_IDENTIFIER];
   --             Loop Statement         end loop [loop_IDENTIFIER];
   --             Package Specification  end [[PARENT_UNIT_NAME .] IDENTIFIER];
   --             Task Definition        end [task_IDENTIFIER];
   --             Protected Definition   end [protected_IDENTIFIER];
   --             Record Definition      end record;
   --
   --           Note that 'e' entries are special in that you get they appear
   --           even in referencing units (normally xref entries appear only
   --           for references in the extended main source unit (see Lib) to
   --           which the ali applies. But 'e' entries are really structural
   --           and simply indicate where packages end. This information can
   --           be used to reconstruct scope information for any entities
   --           referenced from within the package.
   --
   --           t is similarly used to identify the end of a corresponding
   --           body (such a reference always links up with a b reference)
   --
   --             Subprogram Body        end [DESIGNATOR];
   --             Package Body           end [[PARENT_UNIT_NAME .] IDENTIFIER];
   --             Task Body              end [task_IDENTIFIER];
   --             Entry Body             end [entry_IDENTIFIER];
   --             Protected Body         end [protected_IDENTIFIER]
   --             Accept Statement       end [entry_IDENTIFIER]];
   --
   --           Note that in the case of accept statements, there can
   --           be multiple b and T/t entries for the same entity.
   --
   --           [..] is used for generic instantiation references. These
   --           references are present only if the entity in question is
   --           a generic entity, and in that case the [..] contains the
   --           reference for the instantiation. In the case of nested
   --           instantiations, this can be nested [...[...[...]]] etc.
   --           The reference is of the form [file|line] no column is
   --           present since it is assumed that only one instantiation
   --           appears on a single source line. Note that the appearence
   --           of file numbers in such references follows the normal
   --           rules (present only if needed, and resets the current
   --           file for subsequent references).
   --
   --     Examples:
   --
   --        44B5*Flag_Type{boolean} 5r23 6m45 3|9r35 11r56
   --
   --           This line gives references for the publicly visible Boolean
   --           type Flag_Type declared on line 44, column 5. There are four
   --           references
   --
   --              a reference on line 5, column 23 of the current file
   --
   --              a modification on line 6, column 45 of the current file
   --
   --              a reference on line 9, column 35 of unit number 3
   --
   --              a reference on line 11, column 56 of unit number 3
   --
   --        2U13 p3=2:35 5b13 8r4 12r13 12t15
   --
   --           This line gives references for the non-publicly visible
   --           procedure p3 declared on line 2, column 13. This procedure
   --           renames the procedure whose identifier reference is at
   --           line 2 column 35. There are four references:
   --
   --              the corresponding body entity at line 5, column 13,
   --              of the current file.
   --
   --              a reference (e.g. a call) at line 8 column 4 of the
   --              of the current file.
   --
   --              the END line of the body has an explict reference to
   --              the name of the procedure at line 12, column 13.
   --
   --              the body ends at line 12, column 15, just past this label.
   --
   --        16I9*My_Type<2|4I9> 18r8
   --
   --           This line gives references for the publicly visible Integer
   --           derived type My_Type declared on line 16, column 9. It also
   --           gives references to the parent type declared in the unit
   --           number 2 on line 4, column 9. There is one reference:
   --
   --              a reference (e.g. a variable declaration) at line 18 column
   --              4 of the current file.
   --
   --        10I3*Genv{integer} 3|4I10[6|12]
   --
   --           This line gives a reference for the entity Genv in a generic
   --           package. The reference in file 3, line 4, col 10, refers to
   --           an instance of the generic where the instantiation can be
   --           found in file 6 at line 12.
   --
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
   --  identified by a single letter, indicating the entity type. The
   --  following table indicates the letter. A space for an entry is
   --  used for entities that do not appear in the cross-reference table.

   --  For objects, the character * appears in this table. In the xref
   --  listing, this character is replaced by the lower case letter that
   --  corresponds to the type of the object. For example, if a variable
   --  is of a Float type, then, since the type is represented by an
   --  upper case F, the object would be represented by a lower case f.

   --  A special exception is the case of booleans, whose entities are
   --  normal E_Enumeration_Type or E_Enumeration_Subtype entities, but
   --  which appear as B/b in the xref lines, rather than E/e.

   --  For private types, the character + appears in the table. In this
   --  case the kind of the underlying type is used, if available, to
   --  determine the character to use in the xref listing. The listing
   --  will still include a '+' for a generic private type, for example.

   Xref_Entity_Letters : array (Entity_Kind) of Character := (
      E_Void                             => ' ',
      E_Variable                         => '*',
      E_Component                        => '*',
      E_Constant                         => '*',
      E_Discriminant                     => '*',

      E_Loop_Parameter                   => '*',
      E_In_Parameter                     => '*',
      E_Out_Parameter                    => '*',
      E_In_Out_Parameter                 => '*',
      E_Generic_In_Out_Parameter         => '*',

      E_Generic_In_Parameter             => '*',
      E_Named_Integer                    => 'N',
      E_Named_Real                       => 'N',
      E_Enumeration_Type                 => 'E',  -- B for boolean
      E_Enumeration_Subtype              => 'E',  -- B for boolean

      E_Signed_Integer_Type              => 'I',
      E_Signed_Integer_Subtype           => 'I',
      E_Modular_Integer_Type             => 'M',
      E_Modular_Integer_Subtype          => 'M',
      E_Ordinary_Fixed_Point_Type        => 'O',

      E_Ordinary_Fixed_Point_Subtype     => 'O',
      E_Decimal_Fixed_Point_Type         => 'D',
      E_Decimal_Fixed_Point_Subtype      => 'D',
      E_Floating_Point_Type              => 'F',
      E_Floating_Point_Subtype           => 'F',

      E_Access_Type                      => 'P',
      E_Access_Subtype                   => 'P',
      E_Access_Attribute_Type            => 'P',
      E_Allocator_Type                   => ' ',
      E_General_Access_Type              => 'P',

      E_Access_Subprogram_Type           => 'P',
      E_Access_Protected_Subprogram_Type => 'P',
      E_Anonymous_Access_Type            => ' ',
      E_Array_Type                       => 'A',
      E_Array_Subtype                    => 'A',

      E_String_Type                      => 'S',
      E_String_Subtype                   => 'S',
      E_String_Literal_Subtype           => ' ',
      E_Class_Wide_Type                  => 'C',

      E_Class_Wide_Subtype               => 'C',
      E_Record_Type                      => 'R',
      E_Record_Subtype                   => 'R',
      E_Record_Type_With_Private         => 'R',
      E_Record_Subtype_With_Private      => 'R',

      E_Private_Type                     => '+',
      E_Private_Subtype                  => '+',
      E_Limited_Private_Type             => '+',
      E_Limited_Private_Subtype          => '+',
      E_Incomplete_Type                  => '+',

      E_Task_Type                        => 'T',
      E_Task_Subtype                     => 'T',
      E_Protected_Type                   => 'W',
      E_Protected_Subtype                => 'W',
      E_Exception_Type                   => ' ',

      E_Subprogram_Type                  => ' ',
      E_Enumeration_Literal              => 'n',
      E_Function                         => 'V',
      E_Operator                         => 'V',
      E_Procedure                        => 'U',

      E_Entry                            => 'Y',
      E_Entry_Family                     => 'Y',
      E_Block                            => 'q',
      E_Entry_Index_Parameter            => '*',
      E_Exception                        => 'X',

      E_Generic_Function                 => 'v',
      E_Generic_Package                  => 'k',
      E_Generic_Procedure                => 'u',
      E_Label                            => 'L',
      E_Loop                             => 'l',

      E_Package                          => 'K',

      --  The following entities are not ones to which we gather
      --  cross-references, since it does not make sense to do so
      --  (e.g. references to a package are to the spec, not the body)
      --  Indeed the occurrence of the body entity is considered to
      --  be a reference to the spec entity.

      E_Package_Body                     => ' ',
      E_Protected_Object                 => ' ',
      E_Protected_Body                   => ' ',
      E_Task_Body                        => ' ',
      E_Subprogram_Body                  => ' ');

   --  The following table is for information purposes. It shows the
   --  use of each character appearing as an entity type.

   --  letter  lower case usage                UPPER CASE USAGE

   --    a     array object (except string)    array type (except string)
   --    b     Boolean object                  Boolean type
   --    c     class-wide object               class-wide type
   --    d     decimal fixed-point object      decimal fixed-point type
   --    e     non-Boolean enumeration object  non_Boolean enumeration type
   --    f     floating-point object           floating-point type
   --    g     (unused)                        (unused)
   --    h     (unused)                        (unused)
   --    i     signed integer object           signed integer type
   --    j     (unused)                        (unused)
   --    k     generic package                 package
   --    l     label on loop                   label on statement
   --    m     modular integer object          modular integer type
   --    n     enumeration literal             named number
   --    o     ordinary fixed-point object     ordinary fixed-point type
   --    p     access object                   access type
   --    q     label on block                  (unused)
   --    r     record object                   record type
   --    s     string object                   string type
   --    t     task object                     task type
   --    u     generic procedure               procedure
   --    v     generic function or operator    function or operator
   --    w     protected object                protected type
   --    x     (unused)                        exception
   --    y     (unused)                        entry or entry family
   --    z     (unused)                        (unused)

   -----------------
   -- Subprograms --
   -----------------

   procedure Generate_Definition (E : Entity_Id);
   --  Records the definition of an entity

   procedure Generate_Operator_Reference (N : Node_Id);
   --  Node N is an operator node, whose entity has been set. If this entity
   --  is a user defined operator (i.e. an operator not defined in package
   --  Standard), then a reference to the operator is recorded at node N.

   procedure Generate_Reference
     (E       : Entity_Id;
      N       : Node_Id;
      Typ     : Character := 'r';
      Set_Ref : Boolean   := True;
      Force   : Boolean   := False);
   --  This procedure is called to record a reference. N is the location
   --  of the reference and E is the referenced entity. Typ is one of:
   --
   --    'b'  body entity (see below)
   --    'c'  completion of incomplete or private type (see below)
   --    'E'  end of spec (label present)
   --    'e'  end of spec (no label present)
   --    'i'  implicit reference
   --    'm'  modification
   --    'r'  standard reference
   --    'T'  end of body (label present)
   --    't'  end of body (no label present)
   --    'x'  type extension
   --    ' '  dummy reference (see below)
   --
   --  Note: all references to incomplete or private types are to the
   --  original (incomplete or private type) declaration. The full
   --  declaration is treated as a reference with type 'c'.
   --
   --  Note: all references to packages or subprograms are to the entity
   --  for the spec. The entity in the body is treated as a reference
   --  with type 'b'. Similar handling for references to subprogram formals.
   --
   --  The call has no effect if N is not in the extended main source unit.
   --  If N is in the extended main source unit, then the Is_Referenced
   --  flag of E is set. In addition, if appropriate, a cross-reference
   --  entry is made. The entry is made if:
   --
   --    cross-reference collection is enabled
   --    both entity and reference come from source (or Force is True)
   --    the entity is one for which xrefs are appropriate
   --    the type letter is non-blank
   --    the node N is an identifier, defining identifier, or expanded name
   --
   --  If all these conditions are met, then a cross-reference entry is
   --  made for later output when Output_References is called.
   --
   --  Note: the dummy entry is for the convenience of some callers, who
   --  find it easier to pass a space to suppress the entry than to do a
   --  specific test. The call has no effect if the type is a space.
   --
   --  The parameter Set_Ref is normally True, and indicates that in
   --  addition to generating a cross-reference, the Referenced flag
   --  of the specified entity should be set. If this parameter is
   --  False, then setting of the Referenced flag is inhibited.
   --
   --  The parameter Force is set to True to force a reference to be
   --  generated even if Comes_From_Source is false. This is used for
   --  certain implicit references, and also for end label references.

   procedure Output_References;
   --  Output references to the current ali file

end Lib.Xref;
