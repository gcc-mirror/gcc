-- { dg-do compile }

package constructor is
   type R (Name_Length : Natural) is record
      Name     : Wide_String (1..Name_Length);
      Multiple : Boolean;
   end record;
   
   Null_Params : constant R :=
     (Name_Length => 0,
      Name        => "",
      Multiple    => False);
end;
