with Ada.Finalization;
package Limited1_Inner is
   type Limited_Type is new Ada.Finalization.Limited_Controlled with record
      Self : access Limited_Type := Limited_Type'Unchecked_Access;
   end record;
   overriding procedure Finalize (X : in out Limited_Type);

   type Inner_Type (What : Boolean) is record
      case What is
         when False =>
            null;
         when True =>
            L : Limited_Type;
      end case;
   end record;

   function Make_Inner return Inner_Type;
end;
