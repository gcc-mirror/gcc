with Class_Wide4_Pkg;

package Class_Wide4_Pkg2 is

   type Object is limited new
     Class_Wide4_Pkg.Conditional_Interface with
   record
      Val : Integer := 1234;
   end record;

   function Is_Valid
     (This : in Object)
      return Boolean
   is
     (This.Val = 1234);

   function Is_Supported_Data
     (This : in Object;
      Data : not null access Class_Wide4_Pkg.Data_Object'Class)
      return Boolean
   is
     (This.Val = 1234);

   procedure Do_Stuff
     (This : in out Object) is null;

   procedure Do_Stuff_Access
     (This : not null access Object) is null;

end Class_Wide4_Pkg2;
