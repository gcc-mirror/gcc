-- { dg-do run }

with Discr59_Pkg1; use Discr59_Pkg1;

procedure Discr59 is

   function At_Response_Decode return At_Response_Type is
      Fill : At_Response_Type (Alert, 1);
   begin
      return Fill;
   end;

   function Decode return Rec is
      Make : constant At_Response_Type := At_Response_Decode;
      Fill : Rec (At_Response, Make.Kind, Make.Units);
   begin
      return Fill;
   end;

   R : constant Rec := Decode;

begin
   null;
end;
