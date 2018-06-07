with Ada.Containers.Formal_Doubly_Linked_Lists;

package Iter1 is
   package My_Lists is new Ada.Containers.Formal_Doubly_Linked_Lists
     (Element_Type => Integer);

   procedure Dummy (L : My_Lists.List);
end Iter1;
