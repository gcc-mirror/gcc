package elab1 is
  
  -- the forward declaration is the trigger
  type Stream;
  
  type Stream_Ptr is access Stream;
  
  type Stream is array (Positive range <>) of Character;
  
  function Get_Size (S : Stream_Ptr) return Natural;
  
  type Rec (Size : Natural) is
    record
      B : Boolean;
    end record;
  
  My_Desc : constant Stream_Ptr := new Stream'(1 => ' ');
  
  My_Size : constant Natural := Get_Size (My_Desc);
  
  subtype My_Rec is Rec (My_Size);

end;
