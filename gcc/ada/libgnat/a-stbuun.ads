------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    ADA.STRINGS.TEXT_BUFFERS.UNBOUNDED                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;
package Ada.Strings.Text_Buffers.Unbounded with
   Preelaborate
   --  , Nonblocking
   --  , Global => null
is

   type Buffer_Type is new Root_Buffer_Type with private;

   function Get (Buffer : in out Buffer_Type) return String with
      Post'Class => Get'Result'First = 1 and then Current_Indent (Buffer) = 0;

   function Wide_Get (Buffer : in out Buffer_Type) return Wide_String with
      Post'Class => Wide_Get'Result'First = 1
      and then Current_Indent (Buffer) = 0;

   function Wide_Wide_Get
     (Buffer : in out Buffer_Type) return Wide_Wide_String with
      Post'Class => Wide_Wide_Get'Result'First = 1
      and then Current_Indent (Buffer) = 0;

   function Get_UTF_8
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_8_String with
      Post'Class => Get_UTF_8'Result'First = 1
      and then Current_Indent (Buffer) = 0;

   function Wide_Get_UTF_16
     (Buffer : in out Buffer_Type) return UTF_Encoding.UTF_16_Wide_String with
      Post'Class => Wide_Get_UTF_16'Result'First = 1
      and then Current_Indent (Buffer) = 0;

private

   procedure Put_UTF_8_Implementation
     (Buffer : in out Root_Buffer_Type'Class;
      Item : UTF_Encoding.UTF_8_String)
     with Pre => Buffer in Buffer_Type'Class;

   package Mapping is new Output_Mapping (Put_UTF_8_Implementation);

   type Chunk;
   type Chunk_Access is access all Chunk;
   type Chunk (Length : Positive) is record
      Next  : Chunk_Access := null;
      Chars : UTF_Encoding.UTF_8_String (1 .. Length);
   end record;

   type Managed_Chunk_List is new Ada.Finalization.Limited_Controlled with
   record
      First_Chunk : aliased Chunk (64);
      --  First chunk in list is not created by an allocator; it is
      --  large enough to suffice for many common images.

      Current_Chunk : Chunk_Access;
      --  Chunk we are currrently writing to.
      --  Initialized to Managed_Chunk_List.First'Access.
   end record;

   overriding procedure Initialize (List : in out Managed_Chunk_List);
   --  List.Current_Chunk := List.First_Chunk'Unchecked_Access;

   overriding procedure Finalize (List : in out Managed_Chunk_List);
   --  Free any allocated chunks.

   type Buffer_Type is new Mapping.Buffer_Type with record
      List : Managed_Chunk_List;

      Last_Used : Natural := 0;
      --  Index of last used char in List.Current_Chunk.all; 0 if none used.
   end record;

end Ada.Strings.Text_Buffers.Unbounded;
