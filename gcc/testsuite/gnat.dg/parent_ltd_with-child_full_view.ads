package Parent_Ltd_With.Child_Full_View is
   
   type Child_Symbol is new Parent_Ltd_With.Symbol with private;
   type Child_Symbol_Access is access all Child_Symbol;
   
   function New_Child_Symbol return Child_Symbol_Access;

private
   
   type Child_Symbol is new Parent_Ltd_With.Symbol with null record;

end Parent_Ltd_With.Child_Full_View;
