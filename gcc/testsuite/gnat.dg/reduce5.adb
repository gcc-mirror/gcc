-- { dg-do compile }
-- { dg-options "-gnat2022" }

with Ada.Text_IO; use Ada.Text_IO;

procedure Reduce5 is
   subtype Chunk_Number is Natural range 1 .. 8;
   Grid  : array (1 .. 80, 1 .. 100) of Boolean := (others => (others => False));
   Partial_Sum, Partial_Max : array (Chunk_Number) of Natural := (others => 0);
   Partial_Min              : array (Chunk_Number) of Natural := (others => Natural'Last);

begin
   for I in Grid'Range (1) loop
      Grid (I, 1) := (for all J in Grid'Range (2) => Grid (I, J) = True);
   end loop;

   for I in Grid'Range (1) loop
      declare
         True_Count : constant Natural :=
           [for J in Grid'Range(2) => (if Grid (I, J) then 1 else 0)]'Reduce("+",0);
      begin
         Partial_Sum (I) := @ + True_Count;
         Partial_Min (I) := Natural'Min (@, True_Count);
         Partial_Max (I) := Natural'Max (@, True_Count);
      end;
   end loop;

  Put_Line ("Total=" & Natural'Image (Partial_Sum'Reduce ("+", 0)) &
            ", Min=" & Natural'Image (Partial_Min'Reduce(Natural'Min, Natural'Last)) &
            ", Max=" & Natural'Image (Partial_Max'Reduce(Natural'Max, 0)));

  Put_Line ("Total=" & Partial_Sum'Reduce ("+", 0)'Image &
            ", Min=" & Partial_Min'Reduce(Natural'Min, Natural'Last)'Image &
            ", Max=" & Partial_Max'Reduce(Natural'Max, 0)'Image);

end;
