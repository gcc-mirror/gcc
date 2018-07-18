with Unchecked_Conversion;

package Debug10_Pkg is

   type Node_Id is range 0 .. 99_999_999;

   Empty : constant Node_Id := 0;

   subtype Entity_Id is Node_Id;

   type Union_Id is new Integer;

   function My_Is_Entity_Name (N : Node_Id) return Boolean;

   function My_Scalar_Range (Id : Entity_Id) return Node_Id;

   function My_Test (N : Node_Id) return Boolean;

   type Node_Kind is (N_Unused_At_Start, N_Unused_At_End);

   type Entity_Kind is (

      E_Void,
      E_Component,
      E_Constant,
      E_Discriminant,
      E_Loop_Parameter,
      E_Variable,
      E_Out_Parameter,
      E_In_Out_Parameter,
      E_In_Parameter,
      E_Generic_In_Out_Parameter,
      E_Generic_In_Parameter,
      E_Named_Integer,
      E_Named_Real,
      E_Enumeration_Type,
      E_Enumeration_Subtype,
      E_Signed_Integer_Type,
      E_Signed_Integer_Subtype,
      E_Modular_Integer_Type,
      E_Modular_Integer_Subtype,
      E_Ordinary_Fixed_Point_Type,
      E_Ordinary_Fixed_Point_Subtype,
      E_Decimal_Fixed_Point_Type,
      E_Decimal_Fixed_Point_Subtype,
      E_Floating_Point_Type,
      E_Floating_Point_Subtype,
      E_Access_Type,
      E_Access_Subtype,
      E_Access_Attribute_Type,
      E_Allocator_Type,
      E_General_Access_Type,
      E_Access_Subprogram_Type,
      E_Anonymous_Access_Subprogram_Type,
      E_Access_Protected_Subprogram_Type,
      E_Anonymous_Access_Protected_Subprogram_Type,
      E_Anonymous_Access_Type,
      E_Array_Type,
      E_Array_Subtype,
      E_String_Literal_Subtype,
      E_Class_Wide_Type,
      E_Class_Wide_Subtype,
      E_Record_Type,
      E_Record_Subtype,
      E_Record_Type_With_Private,
      E_Record_Subtype_With_Private,
      E_Private_Type,
      E_Private_Subtype,
      E_Limited_Private_Type,
      E_Limited_Private_Subtype,
      E_Incomplete_Type,
      E_Incomplete_Subtype,
      E_Task_Type,
      E_Task_Subtype,
      E_Protected_Type,
      E_Protected_Subtype,
      E_Exception_Type,
      E_Subprogram_Type,
      E_Enumeration_Literal,
      E_Function,
      E_Operator,
      E_Procedure,
      E_Abstract_State,
      E_Entry,
      E_Entry_Family,
      E_Block,
      E_Entry_Index_Parameter,
      E_Exception,
      E_Generic_Function,
      E_Generic_Procedure,
      E_Generic_Package,
      E_Label,
      E_Loop,
      E_Return_Statement,
      E_Package,
      E_Package_Body,
      E_Protected_Object,
      E_Protected_Body,
      E_Task_Body,
      E_Subprogram_Body
   );

   subtype Access_Kind                 is Entity_Kind range
       E_Access_Type ..
       E_Anonymous_Access_Type;

   subtype Array_Kind                  is Entity_Kind range
       E_Array_Type ..
       E_String_Literal_Subtype;

   subtype Object_Kind                 is Entity_Kind range
       E_Component ..
       E_Generic_In_Parameter;

   subtype Record_Kind                 is Entity_Kind range
       E_Class_Wide_Type ..
       E_Record_Subtype_With_Private;

   subtype Scalar_Kind                 is Entity_Kind range
       E_Enumeration_Type ..
       E_Floating_Point_Subtype;

   subtype Type_Kind                   is Entity_Kind range
       E_Enumeration_Type ..
       E_Subprogram_Type;

   type Node_Record (Is_Extension : Boolean := False) is record
      Flag16 : Boolean;
      Nkind : Node_Kind;
   end record;

   function N_To_E is new Unchecked_Conversion (Node_Kind, Entity_Kind);

   type Arr is array (Node_Id) of Node_Record;

   Nodes : Arr;

end Debug10_Pkg;
