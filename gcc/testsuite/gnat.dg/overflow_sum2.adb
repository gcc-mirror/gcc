-- { dg-do compile }
-- { dg-options "-gnato" }

with Namet; use Namet;

function Overflow_Sum2 return Hash_Index_Type is

  Even_Name_Len : Integer;

begin

  if Name_Len > 12 then
    Even_Name_Len := (Name_Len) / 2 * 2;

  return ((((((((((((
    Character'Pos (Name_Buffer (01))) * 2 +
    Character'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
    Character'Pos (Name_Buffer (03))) * 2 +
    Character'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
    Character'Pos (Name_Buffer (05))) * 2 +
    Character'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
    Character'Pos (Name_Buffer (07))) * 2 +
    Character'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
    Character'Pos (Name_Buffer (09))) * 2 +
    Character'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
    Character'Pos (Name_Buffer (11))) * 2 +
    Character'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
  end if;

  return 0;

end;
