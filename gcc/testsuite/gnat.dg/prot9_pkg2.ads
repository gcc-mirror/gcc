with Ada.Containers.Doubly_Linked_Lists;

package Prot9_Pkg2 is

   type Prot_type is limited private;

private

   package My_Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);

   protected type Prot_type is
   private
     L : My_Lists.List;
   end Prot_type;

end Prot9_Pkg2;
