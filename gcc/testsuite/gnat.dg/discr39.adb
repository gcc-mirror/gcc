-- { dg-do run }

with System.Storage_Elements; use System.Storage_Elements;

procedure Discr39 is

  type Rec (Has_Src : Boolean) is record
     case Has_Src is
        when True  => Src : aliased Integer;
        when False => null;
     end case;
  end record;
  pragma Pack(Rec);
  for Rec'Alignment use Integer'Alignment;

  R : Rec (Has_Src => True);

begin
  if R.Src'Address mod Integer'Alignment /= 0 then
     raise Program_Error;
  end if;
end;
