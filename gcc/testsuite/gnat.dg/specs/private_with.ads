--  { dg-do compile }

private with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Deallocation;
package private_with is

   type String_Access is access String;

   package Index_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Positive);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => String,
      Name   => String_Access);
end;
