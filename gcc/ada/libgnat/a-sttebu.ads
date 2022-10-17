------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         ADA.STRINGS.TEXT_BUFFERS                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.UTF_Encoding;
package Ada.Strings.Text_Buffers with
   Pure
is

   type Text_Buffer_Count is range 0 .. Integer'Last;

   New_Line_Count : constant Text_Buffer_Count := 1;
   --  There is no support for two-character CR/LF line endings.

   type Root_Buffer_Type is abstract tagged limited private with
      Default_Initial_Condition => Current_Indent (Root_Buffer_Type) = 0;

   procedure Put (Buffer : in out Root_Buffer_Type; Item : String) is abstract;

   procedure Wide_Put
     (Buffer : in out Root_Buffer_Type; Item : Wide_String) is abstract;

   procedure Wide_Wide_Put
     (Buffer : in out Root_Buffer_Type; Item : Wide_Wide_String) is abstract;

   procedure Put_UTF_8
     (Buffer : in out Root_Buffer_Type;
      Item   :        UTF_Encoding.UTF_8_String) is abstract;

   procedure Wide_Put_UTF_16
     (Buffer : in out Root_Buffer_Type;
      Item   :        UTF_Encoding.UTF_16_Wide_String) is abstract;

   procedure New_Line (Buffer : in out Root_Buffer_Type) is abstract;

   Standard_Indent : constant Text_Buffer_Count := 3;

   function Current_Indent
     (Buffer : Root_Buffer_Type) return Text_Buffer_Count;

   procedure Increase_Indent
     (Buffer : in out Root_Buffer_Type;
      Amount :        Text_Buffer_Count := Standard_Indent) with
      Post'Class => Current_Indent (Buffer) =
      Current_Indent (Buffer)'Old + Amount;

   procedure Decrease_Indent
     (Buffer : in out Root_Buffer_Type;
      Amount :        Text_Buffer_Count := Standard_Indent) with
      Pre'Class => Current_Indent (Buffer) >= Amount
      --  or else raise Constraint_Error,
      or else Boolean'Val (Current_Indent (Buffer) - Amount),
      Post'Class => Current_Indent (Buffer) =
      Current_Indent (Buffer)'Old - Amount;

private

   type Root_Buffer_Type is abstract tagged limited record
      Indentation : Natural := 0;
      --  Current indentation

      Indent_Pending : Boolean := True;
      --  Set by calls to New_Line, cleared when indentation emitted.

      UTF_8_Length : Natural := 0;
      --  Count of UTF_8 characters in the buffer

      UTF_8_Column : Positive := 1;
      --  Column in which next character will be written.
      --  Calling New_Line resets to 1.

      All_7_Bits : Boolean := True;
      --  True if all characters seen so far fit in 7 bits
      All_8_Bits : Boolean := True;
      --  True if all characters seen so far fit in 8 bits

   end record;

   generic
      --  This generic allows a client to extend Root_Buffer_Type without
      --  having to implement any of the abstract subprograms other than
      --  Put_UTF_8 (i.e., Put, Wide_Put, Wide_Wide_Put, Wide_Put_UTF_16,
      --  and New_Line). Without this generic, each client would have to
      --  duplicate the implementations of those 5 subprograms.
      --  This generic also takes care of handling indentation, thereby
      --  avoiding further code duplication. The name "Output_Mapping" isn't
      --  wonderful, but it refers to the idea that this package knows how
      --  to implement all the other output operations in terms of
      --  just Put_UTF_8.
      --
      --  The classwide parameter type here is somewhat tricky;
      --  there are no dispatching calls associated with this parameter.
      --  It would be more accurate to say that the parameter is of type
      --  Output_Mapping.Buffer_Type'Class, but that type hasn't been declared
      --  yet. Instantiators will typically declare a non-abstract extension,
      --  B2, of the buffer type, B1, declared in their instantiation. The
      --  actual Put_UTF_8_Implementation parameter may then have a
      --  precondition "Buffer in B2'Class" and that subprogram can safely
      --  access components declared as part of the declaration of B2.

      with procedure Put_UTF_8_Implementation
        (Buffer : in out Root_Buffer_Type'Class;
         Item   :        UTF_Encoding.UTF_8_String);
   package Output_Mapping is
      type Buffer_Type is abstract new Root_Buffer_Type with null record;

      overriding procedure Put (Buffer : in out Buffer_Type; Item : String);

      overriding procedure Wide_Put
        (Buffer : in out Buffer_Type; Item : Wide_String);

      overriding procedure Wide_Wide_Put
        (Buffer : in out Buffer_Type; Item : Wide_Wide_String);

      overriding procedure Put_UTF_8
        (Buffer : in out Buffer_Type;
         Item   :        UTF_Encoding.UTF_8_String);

      overriding procedure Wide_Put_UTF_16
        (Buffer : in out Buffer_Type; Item : UTF_Encoding.UTF_16_Wide_String);

      overriding procedure New_Line (Buffer : in out Buffer_Type);
   end Output_Mapping;

end Ada.Strings.Text_Buffers;
