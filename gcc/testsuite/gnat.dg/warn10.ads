with Warn10_Pkg; use Warn10_Pkg;

package Warn10 is

   type My_Driver is new Root with record
      Extra : Natural;
   end record;

   procedure Do_Something(Driver : My_Driver);

end Warn10;
