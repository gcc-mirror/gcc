with deref1;
package deref2 is
   type NT is tagged limited private;
   
   function PT_View (Obj : not null access NT)
     return not null access deref1.T'Class;
private
   type PT (Obj : not null access NT) is new deref1.T with null record;
   
   type NT is tagged limited record
      PT_View : aliased PT (NT'Access);
   end record;
end;
