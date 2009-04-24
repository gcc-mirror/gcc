-- { dg-do compile }
-- { dg-options "-gnatwa" }

package Rep_Clause1 is
   generic
      type Custom_T is private;
   package Handler is
      type Storage_T is record
         A : Boolean;
         B : Boolean;
         C : Custom_T;
      end record;

      for Storage_T use record
         A at 0 range 0..0;
         B at 1 range 0..0;
      end record;
   end Handler;
end Rep_Clause1;
