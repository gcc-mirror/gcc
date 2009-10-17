with Slice8_Pkg2;

generic

   with package Str is new Slice8_Pkg2 (<>);

package Slice8_Pkg3 is

   function Get return Str.Paragraph;

end Slice8_Pkg3;
