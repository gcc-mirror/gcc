------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;
with Table;
with System;   use System;
with Types;    use Types;

package Namet is

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file namet.h
--  which is created manually from namet.ads and namet.adb.

--  This package contains routines for handling the names table. The table
--  is used to store character strings for identifiers and operator symbols,
--  as well as other string values such as unit names and file names.

--  The forms of the entries are as follows:

--    Identifiers Stored with upper case letters folded to lower case. Upper
--                       half (16#80# bit set) and wide characters are stored
--                       in an encoded form (Uhh for upper half char, Whhhh
--                       for wide characters, WWhhhhhhhh as provided by the
--                       routine Store_Encoded_Character, where hh are hex
--                       digits for the character code using lower case a-f).
--                       Normally the use of U or W in other internal names is
--                       avoided, but these letters may be used in internal
--                       names (without this special meaning), if they appear
--                       as the last character of the name, or they are
--                       followed by an upper case letter (other than the WW
--                       sequence), or an underscore.


--    Operator symbols   Stored with an initial letter O, and the remainder
--                       of the name is the lower case characters XXX where
--                       the name is Name_Op_XXX, see Snames spec for a full
--                       list of the operator names. Normally the use of O
--                       in other internal names is avoided, but it may be
--                       used in internal names (without this special meaning)
--                       if it is the last character of the name, or if it is
--                       followed by an upper case letter or an underscore.

--    Character literals Character literals have names that are used only for
--                       debugging and error message purposes. The form is a
--                       upper case Q followed by a single lower case letter,
--                       or by a Uxx/Wxxxx/WWxxxxxxx encoding as described for
--                       identifiers. The Set_Character_Literal_Name procedure
--                       should be used to construct these encodings. Normally
--                       the use of O in other internal names is avoided, but
--                       it may be used in internal names (without this special
--                       meaning) if it is the last character of the name, or
--                       if it is followed by an upper case letter or an
--                       underscore.

--    Unit names         Stored with upper case letters folded to lower case,
--                       using Uhh/Whhhh/WWhhhhhhhh encoding as described for
--                       identifiers, and a %s or %b suffix for specs/bodies.
--                       See package Uname for further details.

--    File names         Are stored in the form provided by Osint. Typically
--                       they may include wide character escape sequences and
--                       upper case characters (in non-encoded form). Casing
--                       is also derived from the external environment. Note
--                       that file names provided by Osint must generally be
--                       consistent with the names from Fname.Get_File_Name.

--    Other strings      The names table is also used as a convenient storage
--                       location for other variable length strings such as
--                       error messages etc. There are no restrictions on what
--                       characters may appear for such entries.

--  Note: the encodings Uhh (upper half characters), Whhhh (wide characters),
--  WWhhhhhhhh (wide wide characters) and Qx (character literal names) are
--  described in the spec, since they are visible throughout the system (e.g.
--  in debugging output). However, no code should depend on these particular
--  encodings, so it should be possible to change the encodings by making
--  changes only to the Namet specification (to change these comments) and the
--  body (which actually implements the encodings).

--  The names are hashed so that a given name appears only once in the table,
--  except that names entered with Name_Enter as opposed to Name_Find are
--  omitted from the hash table.

--  The first 26 entries in the names table (with Name_Id values in the range
--  First_Name_Id .. First_Name_Id + 25) represent names which are the one
--  character lower case letters in the range a-z, and these names are created
--  and initialized by the Initialize procedure.

--  Two values, one of type Int and one of type Byte, are stored with each
--  names table entry and subprograms are provided for setting and retrieving
--  these associated values. The usage of these values is up to the client. In
--  the compiler, the Int field is used to point to a chain of potentially
--  visible entities (see Sem.Ch8 for details), and the Byte field is used to
--  hold the Token_Type value for reserved words (see Sem for details). In the
--  binder, the Byte field is unused, and the Int field is used in various
--  ways depending on the name involved (see binder documentation).

   Name_Buffer : String (1 .. 16*1024);
   --  This buffer is used to set the name to be stored in the table for the
   --  Name_Find call, and to retrieve the name for the Get_Name_String call.
   --  The plus 1 in the length allows for cases of adding ASCII.NUL. The 16K
   --  here is intended to be an infinite value that ensures that we never
   --  overflow the buffer (names this long are too absurd to worry!)

   Name_Len : Natural;
   --  Length of name stored in Name_Buffer. Used as an input parameter for
   --  Name_Find, and as an output value by Get_Name_String, or Write_Name.

   -----------------
   -- Subprograms --
   -----------------

   procedure Finalize;
   --  Called at the end of a use of the Namet package (before a subsequent
   --  call to Initialize). Currently this routine is only used to generate
   --  debugging output.

   procedure Get_Name_String (Id : Name_Id);
   --  Get_Name_String is used to retrieve the string associated with an entry
   --  in the names table. The resulting string is stored in Name_Buffer and
   --  Name_Len is set. It is an error to call Get_Name_String with one of the
   --  special name Id values (No_Name or Error_Name).

   function Get_Name_String (Id : Name_Id) return String;
   --  This functional form returns the result as a string without affecting
   --  the contents of either Name_Buffer or Name_Len.

   procedure Get_Unqualified_Name_String (Id : Name_Id);
   --  Similar to the above except that qualification (as defined in unit
   --  Exp_Dbug) is removed (including both preceding __ delimited names, and
   --  also the suffixes used to indicate package body entities and to
   --  distinguish between overloaded entities). Note that names are not
   --  qualified until just before the call to gigi, so this routine is only
   --  needed by processing that occurs after gigi has been called. This
   --  includes all ASIS processing, since ASIS works on the tree written
   --  after gigi has been called.

   procedure Get_Name_String_And_Append (Id : Name_Id);
   --  Like Get_Name_String but the resulting characters are appended to the
   --  current contents of the entry stored in Name_Buffer, and Name_Len is
   --  incremented to include the added characters.

   procedure Get_Decoded_Name_String (Id : Name_Id);
   --  Same calling sequence an interface as Get_Name_String, except that the
   --  result is decoded, so that upper half characters and wide characters
   --  appear as originally found in the source program text, operators have
   --  their source forms (special characters and enclosed in quotes), and
   --  character literals appear surrounded by apostrophes.

   procedure Get_Unqualified_Decoded_Name_String (Id : Name_Id);
   --  Similar to the above except that qualification (as defined in unit
   --  Exp_Dbug) is removed (including both preceding __ delimited names, and
   --  also the suffix used to indicate package body entities). Note that
   --  names are not qualified until just before the call to gigi, so this
   --  routine is only needed by processing that occurs after gigi has been
   --  called. This includes all ASIS processing, since ASIS works on the tree
   --  written after gigi has been called.

   procedure Get_Decoded_Name_String_With_Brackets (Id : Name_Id);
   --  This routine is similar to Decoded_Name, except that the brackets
   --  notation (Uhh replaced by ["hh"], Whhhh replaced by ["hhhh"],
   --  WWhhhhhhhh replaced by ["hhhhhhhh"]) is used for all non-lower half
   --  characters, regardless of how Opt.Wide_Character_Encoding_Method is
   --  set, and also in that characters in the range 16#80# .. 16#FF# are
   --  converted to brackets notation in all cases. This routine can be used
   --  when there is a requirement for a canonical representation not affected
   --  by the character set options (e.g. in the binder generation of
   --  symbols).

   function Get_Name_Table_Byte (Id : Name_Id) return Byte;
   pragma Inline (Get_Name_Table_Byte);
   --  Fetches the Byte value associated with the given name

   function Get_Name_Table_Info (Id : Name_Id) return Int;
   pragma Inline (Get_Name_Table_Info);
   --  Fetches the Int value associated with the given name

   function Is_Operator_Name (Id : Name_Id) return Boolean;
   --  Returns True if name given is of the form of an operator (that
   --  is, it starts with an upper case O).

   procedure Initialize;
   --  Initializes the names table, including initializing the first 26
   --  entries in the table (for the 1-character lower case names a-z) Note
   --  that Initialize must not be called if Tree_Read is used.

   procedure Lock;
   --  Lock name table before calling back end. Space for up to 10 extra
   --  names and 1000 extra characters is reserved before the table is locked.

   procedure Unlock;
   --  Unlocks the name table to allow use of the 10 extra names and 1000
   --  extra characters reserved by the Lock call. See gnat1drv for details of
   --  the need for this.

   function Length_Of_Name (Id : Name_Id) return Nat;
   pragma Inline (Length_Of_Name);
   --  Returns length of given name in characters. This is the length of the
   --  encoded name, as stored in the names table, the result is equivalent to
   --  calling Get_Name_String and reading Name_Len, except that a call to
   --  Length_Of_Name does not affect the contents of Name_Len and Name_Buffer.

   function Name_Chars_Address return System.Address;
   --  Return starting address of name characters table (used in Back_End call
   --  to Gigi).

   function Name_Find return Name_Id;
   --  Name_Find is called with a string stored in Name_Buffer whose length is
   --  in Name_Len (i.e. the characters of the name are in subscript positions
   --  1 to Name_Len in Name_Buffer). It searches the names table to see if
   --  the string has already been stored. If so the Id of the existing entry
   --  is returned. Otherwise a new entry is created with its Name_Table_Info
   --  field set to zero. The contents of Name_Buffer and Name_Len are not
   --  modified by this call. Note that it is permissible for Name_Len to be
   --  set to zero to lookup the null name string.

   function Name_Enter return Name_Id;
   --  Name_Enter has the same calling interface as Name_Find. The difference
   --  is that it does not search the table for an existing match, and also
   --  subsequent Name_Find calls using the same name will not locate the
   --  entry created by this call. Thus multiple calls to Name_Enter with the
   --  same name will create multiple entries in the name table with different
   --  Name_Id values. This is useful in the case of created names, which are
   --  never expected to be looked up. Note: Name_Enter should never be used
   --  for one character names, since these are efficiently located without
   --  hashing by Name_Find in any case.

   function Name_Entries_Address return System.Address;
   --  Return starting address of Names table (used in Back_End call to Gigi)

   function Name_Entries_Count return Nat;
   --  Return current number of entries in the names table

   function Is_OK_Internal_Letter (C : Character) return Boolean;
   pragma Inline (Is_OK_Internal_Letter);
   --  Returns true if C is a suitable character for using as a prefix or a
   --  suffix of an internally generated name, i.e. it is an upper case letter
   --  other than one of the ones used for encoding source names (currently
   --  the set of reserved letters is O, Q, U, W) and also returns False for
   --  the letter X, which is reserved for debug output (see Exp_Dbug).

   function Is_Internal_Name (Id : Name_Id) return Boolean;
   --  Returns True if the name is an internal name (i.e. contains a character
   --  for which Is_OK_Internal_Letter is true, or if the name starts or ends
   --  with an underscore. This call destroys the value of Name_Len and
   --  Name_Buffer (it loads these as for Get_Name_String).
   --
   --  Note: if the name is qualified (has a double underscore), then only the
   --  final entity name is considered, not the qualifying names. Consider for
   --  example that the name:
   --
   --    pkg__B_1__xyz
   --
   --  is not an internal name, because the B comes from the internal name of
   --  a qualifying block, but the xyz means that this was indeed a declared
   --  identifier called "xyz" within this block and there is nothing internal
   --  about that name.

   function Is_Internal_Name return Boolean;
   --  Like the form with an Id argument, except that the name to be tested is
   --  passed in Name_Buffer and Name_Len (which are not affected by the call).
   --  Name_Buffer (it loads these as for Get_Name_String).

   procedure Reset_Name_Table;
   --  This procedure is used when there are multiple source files to reset
   --  the name table info entries associated with current entries in the
   --  names table. There is no harm in keeping the names entries themselves
   --  from one compilation to another, but we can't keep the entity info,
   --  since this refers to tree nodes, which are destroyed between each main
   --  source file.

   procedure Add_Char_To_Name_Buffer (C : Character);
   pragma Inline (Add_Char_To_Name_Buffer);
   --  Add given character to the end of the string currently stored in the
   --  Name_Buffer, incrementing Name_Len.

   procedure Add_Nat_To_Name_Buffer (V : Nat);
   --  Add decimal representation of given value to the end of the string
   --  currently stored in Name_Buffer, incrementing Name_Len as required.

   procedure Add_Str_To_Name_Buffer (S : String);
   --  Add characters of string S to the end of the string currently stored
   --  in the Name_Buffer, incrementing Name_Len by the length of the string.

   procedure Set_Character_Literal_Name (C : Char_Code);
   --  This procedure sets the proper encoded name for the character literal
   --  for the given character code. On return Name_Buffer and Name_Len are
   --  set to reflect the stored name.

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int);
   pragma Inline (Set_Name_Table_Info);
   --  Sets the Int value associated with the given name

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte);
   pragma Inline (Set_Name_Table_Byte);
   --  Sets the Byte value associated with the given name

   procedure Store_Encoded_Character (C : Char_Code);
   --  Stores given character code at the end of Name_Buffer, updating the
   --  value in Name_Len appropriately. Lower case letters and digits are
   --  stored unchanged. Other 8-bit characters are stored using the Uhh
   --  encoding (hh = hex code), other 16-bit wide character values are stored
   --  using the Whhhh (hhhh = hex code) encoding, and other 32-bit wide wide
   --  character values are stored using the WWhhhhhhhh (hhhhhhhh = hex code).
   --  Note that this procedure does not fold upper case letters (they are
   --  stored using the Uhh encoding). If folding is required, it must be done
   --  by the caller prior to the call.

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using the relevant
   --  Table.Tree_Read routines. Note that Initialize should not be called if
   --  Tree_Read is used. Tree_Read includes all necessary initialization.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using the relevant
   --  Table.Tree_Write routines.

   procedure Get_Last_Two_Chars (N : Name_Id; C1, C2 : out Character);
   --  Obtains last two characters of a name. C1 is last but one character
   --  and C2 is last character. If name is less than two characters long,
   --  then both C1 and C2 are set to ASCII.NUL on return.

   procedure Write_Name (Id : Name_Id);
   --  Write_Name writes the characters of the specified name using the
   --  standard output procedures in package Output. No end of line is
   --  written, just the characters of the name. On return Name_Buffer and
   --  Name_Len are set as for a call to Get_Name_String. The name is written
   --  in encoded form (i.e. including Uhh, Whhh, Qx, _op as they appear in
   --  the name table). If Id is Error_Name, or No_Name, no text is output.

   procedure wn (Id : Name_Id);
   pragma Export (Ada, wn);
   --  Like Write_Name, but includes new line at end. Intended for use
   --  from the debugger only.

   procedure Write_Name_Decoded (Id : Name_Id);
   --  Like Write_Name, except that the name written is the decoded name, as
   --  described for Get_Decoded_Name_String, and the resulting value stored
   --  in Name_Len and Name_Buffer is the decoded name.

   ---------------------------
   -- Table Data Structures --
   ---------------------------

   --  The following declarations define the data structures used to store
   --  names. The definitions are in the private part of the package spec,
   --  rather than the body, since they are referenced directly by gigi.

private

   --  This table stores the actual string names. Although logically there is
   --  no need for a terminating character (since the length is stored in the
   --  name entry table), we still store a NUL character at the end of every
   --  name (for convenience in interfacing to the C world).

   package Name_Chars is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Name_Chars_Initial,
     Table_Increment      => Alloc.Name_Chars_Increment,
     Table_Name           => "Name_Chars");

   type Name_Entry is record
      Name_Chars_Index : Int;
      --  Starting location of characters in the Name_Chars table minus one
      --  (i.e. pointer to character just before first character). The reason
      --  for the bias of one is that indexes in Name_Buffer are one's origin,
      --  so this avoids unnecessary adds and subtracts of 1.

      Name_Len : Short;
      --  Length of this name in characters

      Byte_Info : Byte;
      --  Byte value associated with this name

      Hash_Link : Name_Id;
      --  Link to next entry in names table for same hash code

      Int_Info : Int;
      --  Int Value associated with this name
   end record;

   --  This is the table that is referenced by Name_Id entries.
   --  It contains one entry for each unique name in the table.

   package Name_Entries is new Table.Table (
     Table_Component_Type => Name_Entry,
     Table_Index_Type     => Name_Id,
     Table_Low_Bound      => First_Name_Id,
     Table_Initial        => Alloc.Names_Initial,
     Table_Increment      => Alloc.Names_Increment,
     Table_Name           => "Name_Entries");

end Namet;
