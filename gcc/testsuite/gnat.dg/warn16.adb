--  { dg-do compile }
--  { dg-options "-gnatwa" }

procedure Warn16 is

   package Define is
      type Key_Type is record
         Value : Integer := 0;
      end record;

      function "=" (Left  : in Key_Type;
                    Right : in Key_Type)
                    return Boolean;
   end;
   package body Define is
      function "=" (Left  : in Key_Type;
                    Right : in Key_Type)
                    return Boolean is
      begin
         return Left.Value = Right.Value;
      end;
   end;

   generic
      type Key_Type is private;
      with function "=" (Left  : in Key_Type;
                         Right : in Key_Type)
                         return Boolean;
   package Oper is end;

   use type Define.Key_Type; -- !!!

   package Inst is new Oper (Key_Type => Define.Key_Type,
                             "="      => "=");
   pragma Unreferenced (Inst);
begin
   null;
end;
