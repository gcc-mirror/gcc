-- { dg-do compile }
package Attribute_Parsing is
   I : constant Integer := 12345;
   S : constant String := I'Img (1 .. 2);
end Attribute_Parsing;
