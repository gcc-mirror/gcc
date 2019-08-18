--  { dg-do run }
--  { dg-options "-gnata" }

with Ada.Text_IO;

procedure Interface10 is

   type Iface is interface;

   type My_First_Type is new Iface with null record;
   type My_Second_Type is new Iface with null record;

   procedure Do_Test (Object : in Iface'Class) is
   begin
      pragma Assert
        ((Object in My_First_Type) = (Object in My_First_Type'Class));
   end;

   V : My_Second_Type;
begin
   Do_Test (V);
end Interface10;
