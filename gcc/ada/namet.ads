------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with Alloc;
with Hostparm; use Hostparm;
with Table;
with Types;    use Types;

package Namet is

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file namet.h

--  This package contains routines for handling the names table. The table
--  is used to store character strings for identifiers and operator symbols,
--  as well as other string values such as unit names and file names.

--  The forms of the entries are as follows:

--    Identifiers        Stored with upper case letters folded to lower case.
--                       Upper half (16#80# bit set) and wide characters are
--                       stored in an encoded form (Uhh for upper half char,
--                       Whhhh for wide characters, WWhhhhhhhh as provided by
--                       the routine Append_Encoded, where hh are hex
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
--                       debugging and error message purposes. The form is an
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

--  Five values, one of type Int, one of type Byte, and three of type Boolean,
--  are stored with each names table entry and subprograms are provided for
--  setting and retrieving these associated values. The usage of these values
--  is up to the client:

--    In the compiler we have the following uses:

--      The Int field is used to point to a chain of potentially visible
--      entities (see Sem.Ch8 for details).

--      The Byte field is used to hold the Token_Type value for reserved words
--      (see Sem for details).

--      The Boolean1 field is used to mark address clauses to optimize the
--      performance of the Exp_Util.Following_Address_Clause function.

--      The Boolean2 field is used to mark simple names that appear in
--      Restriction[_Warning]s pragmas for No_Use_Of_Entity. This avoids most
--      unnecessary searches of the No_Use_Of_Entity table.

--      The Boolean3 field is set for names of pragmas that are to be ignored
--      because of the occurrence of a corresponding pragma Ignore_Pragma.

--    In the binder, we have the following uses:

--      The Int field is used in various ways depending on the name involved,
--      see binder documentation for details.

--      The Byte and Boolean fields are unused.

--  Note that the value of the Int and Byte fields are initialized to zero,
--  and the Boolean field is initialized to False, when a new Name table entry
--  is created.

   type Bounded_String (Max_Length : Natural := 2**12) is limited
   --  It's unlikely to have names longer than this. But we don't want to make
   --  it too big, because we declare these on the stack in recursive routines.
   record
      Length : Natural := 0;
      Chars  : String (1 .. Max_Length);
   end record;

   --  To create a Name_Id, you can declare a Bounded_String as a local
   --  variable, and Append things onto it, and finally call Name_Find.
   --  You can also use a String, as in:
   --     X := Name_Find (Some_String & "_some_suffix");

   --  For historical reasons, we also have the Global_Name_Buffer below,
   --  which is used by most of the code via the renamings. New code ought
   --  to avoid the global.

   Global_Name_Buffer : Bounded_String (Max_Length => 4 * Max_Line_Length);
   Name_Buffer        : String renames Global_Name_Buffer.Chars;
   Name_Len           : Natural renames Global_Name_Buffer.Length;

   --  Note that there is some circuitry (e.g. Osint.Write_Program_Name) that
   --  does a save/restore on Name_Len and Name_Buffer (1 .. Name_Len). This
   --  works in part because Name_Len is default-initialized to 0.

   procedure Destroy_Global_Name_Buffer with Inline;
   --  Overwrites Global_Name_Buffer with meaningless data. This can be used in
   --  the transition away from Global_Name_Buffer, in order to detect cases
   --  where we incorrectly rely on the global.

   -----------------------------
   -- Types for Namet Package --
   -----------------------------

   --  Name_Id values are used to identify entries in the names table. Except
   --  for the special values No_Name and Error_Name, they are subscript values
   --  for the Names table defined in this package.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type Name_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   type Name_Id is range Names_Low_Bound .. Names_High_Bound;
   for Name_Id'Size use 32;
   --  Type used to identify entries in the names table

   No_Name : constant Name_Id := Names_Low_Bound;
   --  The special Name_Id value No_Name is used in the parser to indicate
   --  a situation where no name is present (e.g. on a loop or block).

   Error_Name : constant Name_Id := Names_Low_Bound + 1;
   --  The special Name_Id value Error_Name is used in the parser to
   --  indicate that some kind of error was encountered in scanning out
   --  the relevant name, so it does not have a representable label.

   First_Name_Id : constant Name_Id := Names_Low_Bound + 2;
   --  Subscript of first entry in names table

   subtype Valid_Name_Id is Name_Id range First_Name_Id .. Name_Id'Last;
   --  All but No_Name and Error_Name

   function Present (Nam : Name_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether name Nam exists

   -----------------
   -- Subprograms --
   -----------------

   function To_String (Buf : Bounded_String) return String;
   pragma Inline (To_String);
   function "+" (Buf : Bounded_String) return String renames To_String;

   function Name_Find
     (Buf : Bounded_String := Global_Name_Buffer) return Valid_Name_Id;
   function Name_Find (S : String) return Valid_Name_Id;
   --  Name_Find searches the names table to see if the string has already been
   --  stored. If so, the Id of the existing entry is returned. Otherwise a new
   --  entry is created with its Name_Table_Int fields set to zero/false. Note
   --  that it is permissible for Buf.Length to be zero to lookup the empty
   --  name string.

   function Name_Enter
     (Buf : Bounded_String := Global_Name_Buffer) return Valid_Name_Id;
   function Name_Enter (S : String) return Valid_Name_Id;
   --  Name_Enter is similar to Name_Find. The difference is that it does not
   --  search the table for an existing match, and also subsequent Name_Find
   --  calls using the same name will not locate the entry created by this
   --  call. Thus multiple calls to Name_Enter with the same name will create
   --  multiple entries in the name table with different Name_Id values. This
   --  is useful in the case of created names, which are never expected to be
   --  looked up. Note: Name_Enter should never be used for one character
   --  names, since these are efficiently located without hashing by Name_Find
   --  in any case.

   function Name_Equals
     (N1 : Valid_Name_Id;
      N2 : Valid_Name_Id) return Boolean;
   --  Return whether N1 and N2 denote the same character sequence

   function Get_Name_String (Id : Valid_Name_Id) return String;
   --  Returns the characters of Id as a String. The lower bound is 1.

   --  The following Append procedures ignore any characters that don't fit in
   --  Buf.

   procedure Append (Buf : in out Bounded_String; C : Character);
   --  Append C onto Buf
   pragma Inline (Append);

   procedure Append (Buf : in out Bounded_String; V : Nat);
   --  Append decimal representation of V onto Buf

   procedure Append (Buf : in out Bounded_String; S : String);
   --  Append S onto Buf

   procedure Append (Buf : in out Bounded_String; Buf2 : Bounded_String);
   --  Append Buf2 onto Buf

   procedure Append (Buf : in out Bounded_String; Id : Valid_Name_Id);
   --  Append the characters of Id onto Buf. It is an error to call this with
   --  one of the special name Id values (No_Name or Error_Name).

   procedure Append_Decoded (Buf : in out Bounded_String; Id : Valid_Name_Id);
   --  Same as Append, except that the result is decoded, so that upper half
   --  characters and wide characters appear as originally found in the source
   --  program text, operators have their source forms (special characters and
   --  enclosed in quotes), and character literals appear surrounded by
   --  apostrophes.

   procedure Append_Decoded_With_Brackets
     (Buf : in out Bounded_String;
      Id  : Valid_Name_Id);
   --  Same as Append_Decoded, except that the brackets notation (Uhh
   --  replaced by ["hh"], Whhhh replaced by ["hhhh"], WWhhhhhhhh replaced by
   --  ["hhhhhhhh"]) is used for all non-lower half characters, regardless of
   --  how Opt.Wide_Character_Encoding_Method is set, and also in that
   --  characters in the range 16#80# .. 16#FF# are converted to brackets
   --  notation in all cases. This routine can be used when there is a
   --  requirement for a canonical representation not affected by the
   --  character set options (e.g. in the binder generation of symbols).

   procedure Append_Unqualified
     (Buf : in out Bounded_String; Id : Valid_Name_Id);
   --  Same as Append, except that qualification (as defined in unit
   --  Exp_Dbug) is removed (including both preceding __ delimited names, and
   --  also the suffixes used to indicate package body entities and to
   --  distinguish between overloaded entities). Note that names are not
   --  qualified until just before the call to gigi, so this routine is only
   --  needed by processing that occurs after gigi has been called.

   procedure Append_Unqualified_Decoded
     (Buf : in out Bounded_String;
      Id  : Valid_Name_Id);
   --  Same as Append_Unqualified, but decoded as for Append_Decoded

   procedure Append_Encoded (Buf : in out Bounded_String; C : Char_Code);
   --  Appends given character code at the end of Buf. Lower case letters and
   --  digits are stored unchanged. Other 8-bit characters are stored using the
   --  Uhh encoding (hh = hex code), other 16-bit wide character values are
   --  stored using the Whhhh (hhhh = hex code) encoding, and other 32-bit wide
   --  wide character values are stored using the WWhhhhhhhh (hhhhhhhh = hex
   --  code). Note that this procedure does not fold upper case letters (they
   --  are stored using the Uhh encoding).

   procedure Set_Character_Literal_Name
     (Buf : in out Bounded_String;
      C   : Char_Code);
   --  This procedure sets the proper encoded name for the character literal
   --  for the given character code.

   procedure Insert_Str
     (Buf   : in out Bounded_String;
      S     : String;
      Index : Positive);
   --  Inserts S in Buf, starting at Index. Any existing characters at or past
   --  this location get moved beyond the inserted string.

   function Is_Internal_Name (Buf : Bounded_String) return Boolean;

   procedure Get_Last_Two_Chars
     (N  : Valid_Name_Id;
      C1 : out Character;
      C2 : out Character);
   --  Obtains last two characters of a name. C1 is last but one character and
   --  C2 is last character. If name is less than two characters long then both
   --  C1 and C2 are set to ASCII.NUL on return.

   function Get_Name_Table_Boolean1 (Id : Valid_Name_Id) return Boolean;
   function Get_Name_Table_Boolean2 (Id : Valid_Name_Id) return Boolean;
   function Get_Name_Table_Boolean3 (Id : Valid_Name_Id) return Boolean;
   --  Fetches the Boolean values associated with the given name

   function Get_Name_Table_Byte (Id : Valid_Name_Id) return Byte;
   pragma Inline (Get_Name_Table_Byte);
   --  Fetches the Byte value associated with the given name

   function Get_Name_Table_Int (Id : Valid_Name_Id) return Int;
   pragma Inline (Get_Name_Table_Int);
   --  Fetches the Int value associated with the given name

   procedure Set_Name_Table_Boolean1 (Id : Valid_Name_Id; Val : Boolean);
   procedure Set_Name_Table_Boolean2 (Id : Valid_Name_Id; Val : Boolean);
   procedure Set_Name_Table_Boolean3 (Id : Valid_Name_Id; Val : Boolean);
   --  Sets the Boolean value associated with the given name

   procedure Set_Name_Table_Byte (Id : Valid_Name_Id; Val : Byte);
   pragma Inline (Set_Name_Table_Byte);
   --  Sets the Byte value associated with the given name

   procedure Set_Name_Table_Int (Id : Valid_Name_Id; Val : Int);
   pragma Inline (Set_Name_Table_Int);
   --  Sets the Int value associated with the given name

   function Is_Internal_Name (Id : Valid_Name_Id) return Boolean;
   --  Returns True if the name is an internal name, i.e. contains a character
   --  for which Is_OK_Internal_Letter is true, or if the name starts or ends
   --  with an underscore.
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

   function Is_OK_Internal_Letter (C : Character) return Boolean;
   pragma Inline (Is_OK_Internal_Letter);
   --  Returns true if C is a suitable character for using as a prefix or a
   --  suffix of an internally generated name, i.e. it is an upper case letter
   --  other than one of the ones used for encoding source names (currently the
   --  set of reserved letters is O, Q, U, W) and also returns False for the
   --  letter X, which is reserved for debug output (see Exp_Dbug).

   function Is_Operator_Name (Id : Valid_Name_Id) return Boolean;
   --  Returns True if name given is of the form of an operator (that is, it
   --  starts with an upper case O).

   function Is_Valid_Name (Id : Name_Id) return Boolean;
   --  True if Id is a valid name - points to a valid entry in the Name_Entries
   --  table.

   function Length_Of_Name (Id : Valid_Name_Id) return Nat;
   pragma Inline (Length_Of_Name);
   --  Returns length of given name in characters. This is the length of the
   --  encoded name, as stored in the names table.

   procedure Initialize;
   --  This is a dummy procedure. It is retained for easy compatibility with
   --  clients who used to call Initialize when this call was required. Now
   --  initialization is performed automatically during package elaboration.
   --  Note that this change fixes problems which existed prior to the change
   --  of Initialize being called more than once. See also Reinitialize which
   --  allows reinitialization of the tables.

   procedure Reinitialize;
   --  Clears the name tables and removes all existing entries from the table.

   procedure Reset_Name_Table;
   --  This procedure is used when there are multiple source files to reset the
   --  name table info entries associated with current entries in the names
   --  table. There is no harm in keeping the names entries themselves from one
   --  compilation to another, but we can't keep the entity info, since this
   --  refers to tree nodes, which are destroyed between each main source file.

   procedure Finalize;
   --  Called at the end of a use of the Namet package (before a subsequent
   --  call to Initialize). Currently this routine is only used to generate
   --  debugging output.

   procedure Lock;
   --  Lock name tables before calling back end. We reserve some extra space
   --  before locking to avoid unnecessary inefficiencies when we unlock.

   procedure Unlock;
   --  Unlocks the name table to allow use of the extra space reserved by the
   --  call to Lock. See gnat1drv for details of the need for this.

   procedure Write_Name (Id : Valid_Name_Id);
   --  Write_Name writes the characters of the specified name using the
   --  standard output procedures in package Output. The name is written
   --  in encoded form (i.e. including Uhh, Whhh, Qx, _op as they appear in
   --  the name table). If Id is Error_Name or No_Name, no text is output.

   procedure Write_Name_Decoded (Id : Valid_Name_Id);
   --  Like Write_Name, except that the name written is the decoded name, as
   --  described for Append_Decoded.

   procedure Write_Name_For_Debug (Id : Name_Id; Quote : String := "");
   --  Like Write_Name, except it tries to be robust in the presence of invalid
   --  data, and valid names are surrounded by Quote.

   function Name_Entries_Count return Nat;
   --  Return current number of entries in the names table

   function Last_Name_Id return Name_Id;
   --  Return the last Name_Id in the table. This information is valid until
   --  new names have been added.

   --------------------------
   -- Obsolete Subprograms --
   --------------------------

   --  The following routines operate on Global_Name_Buffer. New code should
   --  use the routines above, and declare Bounded_Strings as local
   --  variables. Existing code can be improved incrementally by removing calls
   --  to the following. If we eliminate all of these, we can remove
   --  Global_Name_Buffer. But be sure to look at namet.h first.

   --  To see what these do, look at the bodies. They are all trivially defined
   --  in terms of routines above.

   procedure Add_Char_To_Name_Buffer (C : Character);
   pragma Inline (Add_Char_To_Name_Buffer);

   procedure Add_Nat_To_Name_Buffer (V : Nat);

   procedure Add_Str_To_Name_Buffer (S : String);

   procedure Get_Decoded_Name_String (Id : Valid_Name_Id);

   procedure Get_Decoded_Name_String_With_Brackets (Id : Valid_Name_Id);

   procedure Get_Name_String (Id : Valid_Name_Id);

   procedure Get_Name_String_And_Append (Id : Valid_Name_Id);

   procedure Get_Unqualified_Decoded_Name_String (Id : Valid_Name_Id);

   procedure Get_Unqualified_Name_String (Id : Valid_Name_Id);

   procedure Insert_Str_In_Name_Buffer (S : String; Index : Positive);

   function Is_Internal_Name return Boolean;

   procedure Set_Character_Literal_Name (C : Char_Code);

   procedure Store_Encoded_Character (C : Char_Code);

   ------------------------------
   -- File and Unit Name Types --
   ------------------------------

   --  These are defined here in Namet rather than Fname and Uname to avoid
   --  problems with dependencies, and to avoid dragging in Fname and Uname
   --  into many more files, but it would be cleaner to move to Fname/Uname.

   type File_Name_Type is new Name_Id;
   --  File names are stored in the names table and this type is used to
   --  indicate that a Name_Id value is being used to hold a simple file name
   --  (which does not include any directory information).

   No_File : constant File_Name_Type := File_Name_Type (No_Name);
   --  Constant used to indicate no file is present (this is used for example
   --  when a search for a file indicates that no file of the name exists).

   function Present (Nam : File_Name_Type) return Boolean;
   pragma Inline (Present);
   --  Determine whether file name Nam exists

   Error_File_Name : constant File_Name_Type := File_Name_Type (Error_Name);
   --  The special File_Name_Type value Error_File_Name is used to indicate
   --  a unit name where some previous processing has found an error.

   subtype Error_File_Name_Or_No_File is
     File_Name_Type range No_File .. Error_File_Name;
   --  Used to test for either error file name or no file

   type Path_Name_Type is new Name_Id;
   --  Path names are stored in the names table and this type is used to
   --  indicate that a Name_Id value is being used to hold a path name (that
   --  may contain directory information).

   No_Path : constant Path_Name_Type := Path_Name_Type (No_Name);
   --  Constant used to indicate no path name is present

   type Unit_Name_Type is new Name_Id;
   --  Unit names are stored in the names table and this type is used to
   --  indicate that a Name_Id value is being used to hold a unit name, which
   --  terminates in %b for a body or %s for a spec.

   No_Unit_Name : constant Unit_Name_Type := Unit_Name_Type (No_Name);
   --  Constant used to indicate no file name present

   function Present (Nam : Unit_Name_Type) return Boolean;
   pragma Inline (Present);
   --  Determine whether unit name Nam exists

   Error_Unit_Name : constant Unit_Name_Type := Unit_Name_Type (Error_Name);
   --  The special Unit_Name_Type value Error_Unit_Name is used to indicate
   --  a unit name where some previous processing has found an error.

   subtype Error_Unit_Name_Or_No_Unit_Name is
     Unit_Name_Type range No_Unit_Name .. Error_Unit_Name;

   ------------------------
   -- Debugging Routines --
   ------------------------

   procedure wn (Id : Name_Id);
   pragma Export (Ada, wn);
   --  Write Id to standard output, followed by a newline. Intended to be
   --  called in the debugger.

private

   ---------------------------
   -- Table Data Structures --
   ---------------------------

   --  The following declarations define the data structures used to store
   --  names. The definitions are in the private part of the package spec,
   --  rather than the body, since they are referenced directly by gigi.

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
      Name_Chars_Index : aliased Int;
      --  Starting location of characters in the Name_Chars table minus one
      --  (i.e. pointer to character just before first character). The reason
      --  for the bias of one is that indexes in Name_Buffer are one's origin,
      --  so this avoids unnecessary adds and subtracts of 1.

      Name_Len : aliased Short;
      --  Length of this name in characters

      Byte_Info : aliased Byte;
      --  Byte value associated with this name

      Name_Has_No_Encodings : Boolean;
      --  This flag is set True if the name entry is known not to contain any
      --  special character encodings. This is used to speed up repeated calls
      --  to Append_Decoded. A value of False means that it is not known
      --  whether the name contains any such encodings.

      Boolean1_Info : Boolean;
      Boolean2_Info : Boolean;
      Boolean3_Info : Boolean;
      --  Boolean values associated with the name

      Spare : Boolean;
      --  Four remaining bits in the current byte

      Hash_Link : aliased Name_Id;
      --  Link to next entry in names table for same hash code

      Int_Info : aliased Int;
      --  Int Value associated with this name

   end record;

   for Name_Entry use record
      Name_Chars_Index      at  0 range 0 .. 31;
      Name_Len              at  4 range 0 .. 15;
      Byte_Info             at  6 range 0 .. 7;
      Name_Has_No_Encodings at  7 range 0 .. 0;
      Boolean1_Info         at  7 range 1 .. 1;
      Boolean2_Info         at  7 range 2 .. 2;
      Boolean3_Info         at  7 range 3 .. 3;
      Spare                 at  7 range 4 .. 7;
      Hash_Link             at  8 range 0 .. 31;
      Int_Info              at 12 range 0 .. 31;
   end record;

   for Name_Entry'Size use 16 * 8;
   --  This ensures that we did not leave out any fields

   --  This is the table that is referenced by Valid_Name_Id entries.
   --  It contains one entry for each unique name in the table.

   package Name_Entries is new Table.Table (
     Table_Component_Type => Name_Entry,
     Table_Index_Type     => Valid_Name_Id'Base,
     Table_Low_Bound      => First_Name_Id,
     Table_Initial        => Alloc.Names_Initial,
     Table_Increment      => Alloc.Names_Increment,
     Table_Name           => "Name_Entries");

end Namet;
