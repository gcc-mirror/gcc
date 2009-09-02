-- { dg-do compile }
-- { dg-options "-gnatws" }

with System.Storage_Elements; use System.Storage_Elements;
with Unchecked_Conversion;
with Slice7_Pkg; use Slice7_Pkg;

procedure Slice7 is

  type Discrete_Type is range 1 .. 32;

  Max_Byte_Count : constant := 4;
  subtype Byte_Count_Type is Storage_Offset range 1..Max_Byte_Count;

  subtype Buffer_Type is Storage_Array (Byte_Count_Type);
  function Convert_Put is new Unchecked_Conversion (Integer, Buffer_Type);

  function Set_Buffer_Size return Byte_Count_Type is
  begin
    return 4;
  end;

  Buffer_Size  : constant Byte_Count_Type := Set_Buffer_Size;
  Buffer_End   : constant Byte_Count_Type := Max_Byte_Count;
  Buffer_Start : constant Byte_Count_Type := Buffer_End - Buffer_Size + 1;

  Obj : Discrete_Type;

begin
  Put (Convert_Put(Discrete_Type'Pos (Obj)));

  Put (Convert_Put(Discrete_Type'Pos (Obj))
       (Buffer_Start..Buffer_End));

  Put (Convert_Put(Discrete_Type'Pos (Obj) -
                   Discrete_Type'Pos (Discrete_Type'First))
       (Buffer_Start..Buffer_End));
end;
