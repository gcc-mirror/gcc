-- { dg-do compile }
-- { dg-options "-gnatws" }

with Array14_Pkg; use Array14_Pkg;

package body Array14 is

  package Nested is

    Length : constant SSE.Storage_Count := Length2;

    subtype Encoded_Index_Type is SSE.Storage_Count range 1 .. Length;
    subtype Encoded_Type is SSE.Storage_Array (Encoded_Index_Type'Range);

    procedure Encode (Input : in Integer; Output : out Encoded_Type);

  end;

  package body Nested is

    procedure Encode (Input : in Integer; Output : out Encoded_Type) is
    begin
      Encode2 (Input, Output);
    end;

  end;

  procedure Init is
    O : Nested.Encoded_Type;
    for O'Alignment use 4;
  begin
    null;
  end;

end Array14;
