--  PR ada/114398
--  Testcase by Dennis van Raaij <d.van.raaij@gmail.com>

--  { dg-do run }

with Ada.Finalization;

procedure Access11 is

   package Pkg is

      type Int is
        new Ada.Finalization.Limited_Controlled
      with record
         Value : Integer;
      end record;

      procedure Set (This : out Int; To : Integer);
      procedure Set (This : out Int; To : Int);

      function "+" (Left, Right : Int) return Int;

      overriding procedure Initialize (This : in out Int);
      overriding procedure Finalize   (This : in out Int);

   end Pkg;

   package body Pkg is

      procedure Set (This : out Int; To : Integer) is
      begin
         This.Value := To;
      end Set;

      procedure Set (This  : out Int; To : Int) is
      begin
         This.Value := To.Value;
      end Set;

      function "+" (Left, Right : Int) return Int is
      begin
         return Result : Int do
            Result.Value := Left.Value + Right.Value;
         end return;
      end "+";

      overriding procedure Initialize (This : in out Int) is
      begin
         This.Value := 42;
      end Initialize;

      overriding procedure Finalize (This : in out Int) is
      begin
         This.Value := 0;
      end Finalize;

   end Pkg;

   use Pkg;

   type Binary_Operator is access
     function (Left, Right : Int) return Int;

   procedure Test
     (Op          : Binary_Operator;
      Left, Right : Int)
   is
      Result : Int;
   begin
      Result.Set (Op (Left, Right));
   end Test;

   A, B : Int;

begin
   A.Set (7);
   B.Set (9);

   Test ("+"'Access, A, B);
end;
