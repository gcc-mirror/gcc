limited with Parent_Ltd_With.Child_Full_View;

package Parent_Ltd_With is
   
   type Symbol is abstract tagged limited private;
   
   type Symbol_Access is access all Symbol'Class;

private
   
   type Symbol is abstract tagged limited record
      Comp : Integer;
   end record;

end Parent_Ltd_With;
