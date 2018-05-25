--  { dg-do run }
--  { dg-options "-gnatws" }

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;
with System.Parameters;      use System.Parameters;
with System.Secondary_Stack; use System.Secondary_Stack;

procedure Sec_Stack2 is
   procedure Overflow_SS_Index;
   --  Create a scenario where the frame index of the secondary stack overflows
   --  while the stack itself uses little memory.

   -----------------------
   -- Overflow_SS_Index --
   -----------------------

   procedure Overflow_SS_Index is
      Max_Iterations : constant := 20_000;
      --  The approximate number of iterations needed to overflow the frame
      --  index type on a 64bit target.

      Algn : constant Positive := Positive (Standard'Maximum_Alignment);
      --  The maximum alignment of the target

      Size : constant Positive := Positive (Runtime_Default_Sec_Stack_Size);
      --  The default size of the secondary stack on the target

      Base_Str : constant String (1 .. Size) := (others => 'a');
      --  A string big enough to fill the static frame of the secondary stack

      Small_Str : constant String (1 .. Algn) := (others => 'a');
      --  A string small enough to cause a new round up to the nearest multiple
      --  of the maximum alignment on the target at each new iteration of the
      --  loop.

      Base_US : Unbounded_String := To_Unbounded_String (Base_Str);
      --  Unbounded version of the base string

      procedure SS_Print is new SS_Info (Put_Line);

   begin
      for Iteration in 1 .. Max_Iterations loop

          --  Grow the base string by a small amount at each iteration of the
          --  loop.

          Append (Base_US, Small_Str);

          --  Convert the unbounded base into a new base. This causes routine
          --  To_String to allocates the new base on the secondary stack. Since
          --  the new base is slignly bigger than the previous base, the stack
          --  would have to create a new frame.

          --  Due to an issue with frame reclamation, the last frame (which is
          --  also not big enough to fit the new base) is never reclaimed. This
          --  causes the range of the new frame to shift toward the overflow
          --  point of the frame index type.

          begin
             declare
                New_Base_Str : constant String := To_String (Base_US);
             begin null; end;

          exception
             when Storage_Error =>
                Put_Line ("ERROR: SS depleted");
                Put_Line ("Iteration:" & Iteration'Img);
                Put_Line ("SS_Size  :" & Size'Img);
                Put_Line ("SS_Algn  :" & Algn'Img);

                SS_Print;
                exit;

             when others =>
                Put_Line ("ERROR: unexpected exception");
                exit;
          end;
      end loop;
   end Overflow_SS_Index;

--  Start of processing for SS_Depletion

begin
   --  This issue manifests only on targets with a dynamic secondary stack

   if Sec_Stack_Dynamic then
      Overflow_SS_Index;
   end if;
end Sec_Stack2;
