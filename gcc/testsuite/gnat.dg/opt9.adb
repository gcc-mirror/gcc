-- { dg-do compile }
-- { dg-options "-gnatws -O" }

with Opt9_Pkg; use Opt9_Pkg;

procedure Opt9 is

   type Array_T is array (1 .. N) of Integer;

   type Clock_T is record
      N_Ticks : Integer := 0;
   end record;

   type Rec is record
      Values : Array_T;
      Valid  : Boolean;
      Tstamp : Clock_T;
   end record;

   pragma Pack (Rec);

   Data : Rec;

begin
   null;
end;
