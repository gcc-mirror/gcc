-- { dg-do compile }

with System, Ada.Unchecked_Conversion;
procedure alignment3 is
   
   type Value_Type (Is_Short : Boolean) is record
      case Is_Short is
         when True =>   V : Natural;
         when others => A, B : Natural;
      end case;
   end record;
   
   type Link_Type (Short_Values : Boolean) is record
      Input, Output : Value_Type (Short_Values);
      Initialized : Boolean;
      N_Probes    : Natural;
   end record;
   
   type Link_Access is access Link_Type;
   
   type Natural_Access is access all Natural;
   function To_Natural_Access is
      new Ada.Unchecked_Conversion (System.Address, Natural_Access);
   
   Ptr : Natural_Access;
   
   procedure N_Probes_For (Link : Link_Access)  is
   begin
      Ptr := To_Natural_Access (Link.N_Probes'address);
      Ptr := To_Natural_Access (Link.Initialized'address);
   end;

begin
   null;
end;
