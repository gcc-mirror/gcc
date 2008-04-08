--  { dg-do compile }

package body Parent_Ltd_With.Child_Full_View is
   
   function New_Child_Symbol return Child_Symbol_Access is
      Sym : constant Child_Symbol_Access := new Child_Symbol'(Comp => 10);
   
   begin
      return Sym;
   end New_Child_Symbol;

end Parent_Ltd_With.Child_Full_View;
