package Class_Wide4_Pkg is

   type Conditional_Interface is limited interface;

   type Data_Object is tagged null record;

   function Is_Valid
     (This : in Conditional_Interface)
      return Boolean is abstract;

   procedure Do_Stuff
     (This : in out Conditional_Interface) is abstract
     with
       Pre'Class => This.Is_Valid;

   procedure Do_Stuff_Access
     (This : not null access Conditional_Interface) is abstract
     with
       Pre'Class => This.Is_Valid;

end Class_Wide4_Pkg;
