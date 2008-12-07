package Aggr10_Pkg is

   type Name_Id is range 300_000_000 .. 399_999_999;
   type Int is range -2 ** 31 .. +2 ** 31 - 1;
   type Source_Id is range 5_000_000 .. 5_999_999;

   type Name_Location is record
      Name     : Name_Id;
      Location : Int;
      Source   : Source_Id;
      Except   : Boolean;
      Found    : Boolean := False;
   end record;

   function Get return Name_Location;
   procedure Set (Name_Loc : Name_Location);

end Aggr10_Pkg;
