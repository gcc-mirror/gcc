--  { dg-do compile }

procedure Self1 is      
   type Event;
        
   type Link (E : access Event) is limited record
      Val : Integer;
   end record;

   type Ptr is access all Event;
   
   type Event is tagged limited record
      Inner : Link (Event'access);
      Size  : Integer;
   end record;              
        
   Obj2 : Ptr := new Event'(Inner => (Event'access, 15),
                            Size => Link'size);
begin
   null;                    
end;    
