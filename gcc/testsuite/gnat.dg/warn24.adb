--  { dg-do compile }

procedure Warn24 is
   type List_D (D : Boolean);

   type List_Acc is access List_D;

   type List_D (D : Boolean) is record
      Next : List_Acc (D);
   end record;

   X : List_D (True);
begin
   X.Next := new List_D (False);
end Warn24;
