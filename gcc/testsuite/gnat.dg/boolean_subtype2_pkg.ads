package Boolean_Subtype2_Pkg is

   type Node_Id is range 0 .. 099_999_999;
   subtype Entity_Id is Node_Id;

   function Node20 (N : Node_Id) return Node_Id;
   function Flag63 (N : Node_Id) return Boolean;
   function Present (N : Node_Id) return Boolean;

end Boolean_Subtype2_Pkg;
