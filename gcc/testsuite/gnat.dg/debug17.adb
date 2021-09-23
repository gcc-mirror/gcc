-- { dg-do compile }
-- { dg-skip-if "No Dwarf" { { hppa*-*-hpux* } && { ! lp64 } } }
-- { dg-options "-cargs -O0 -g -dA -fgnat-encodings=minimal -margs" }

pragma No_Component_Reordering;

procedure Debug17 (Number_Of_Bits : Natural) is

   type Bitinfos_T is array (Natural range 1 .. Number_Of_Bits) of Float;

   type Inner_Record_T is record
      Bitinfos : Bitinfos_T := (others => 1.5);
      Check1 : Integer := 1983;
      Check2 : Integer := 1995;
      Flag : Boolean := False;
      Check3 : Integer := 2005;
   end record;

   Rfield : Inner_Record_T;

begin
   null;
end;

-- { dg-final { scan-assembler-not "DW_AT_data_member_location (0)" } }
