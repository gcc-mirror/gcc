--  { dg-do compile }

with Ada.Text_IO;

package body Iter1 is

   type Table is array (Integer range <>) of Float;
   My_Table : Table := (1.0, 2.0, 3.0);

   procedure Dummy (L : My_Lists.List) is
   begin
      for Item : Boolean of L loop --  { dg-error "subtype indication does not match element type" }
         Ada.Text_IO.Put_Line (Integer'Image (Item));
      end loop;

      for Item : Boolean of My_Table loop --  { dg-error "subtype indication does not match component type" }
         null;
      end loop;
   end;
end Iter1;
