-- { dg-do run }

procedure Pack19 is

   subtype Always_False is Boolean range False .. False;

   type Rec1 is record
      B1 : Boolean;
      B2 : Boolean;
      B3 : Boolean;
      B4 : Boolean;
      B5 : Boolean;
      B6 : Boolean;
      B7 : Always_False;
      B8 : Boolean;
   end record;
   pragma Pack (Rec1);

   subtype Always_True is Boolean range True .. True;

   type Rec2 is record
      B1 : Boolean;
      B2 : Boolean;
      B3 : Boolean;
      B4 : Boolean;
      B5 : Boolean;
      B6 : Boolean;
      B7 : Always_True;
      B8 : Boolean;
   end record;
   pragma Pack (Rec2);

   R1 : Rec1 := (True, True, True, True, True, True, False, False);
   R2 : Rec2 := (False, False, False, False, False, False, True, True);

begin
   R1.B8 := True;
   if R1.B7 /= False then
      raise Program_Error;
   end if;

   R1.B7 := False;
   if R1.B7 /= False then
      raise Program_Error;
   end if;

   R2.B8 := False;
   if R2.B7 /= True then
      raise Program_Error;
   end if;

   R2.B7 := True;
   if R2.B7 /= True then
      raise Program_Error;
   end if;
end;
