with Ada.Containers.Doubly_Linked_Lists;
with Equal11_Interface;

package Equal11_Record is

  use Equal11_Interface;

  type My_Record_Type is new My_Interface_Type with
    record
      F : Integer;
    end record;

  overriding
  procedure Put (R : in My_Record_Type);

  Put_Result : Integer;

   package My_Record_Type_List_Pck is
     new Ada.Containers.Doubly_Linked_Lists (Element_Type => My_Record_Type);

end Equal11_Record;
