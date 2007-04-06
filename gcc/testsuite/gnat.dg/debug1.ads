package debug1 is
  
  type Vector is array (Natural range <>) of Natural;
  type Vector_Access is access Vector;
        
  type Data_Line is record
    Length : Vector (1 .. 1);
    Line   : Vector_Access;
  end record;
  
  type Data_Block is array (1 .. 5) of Data_Line;
  type Data_Block_Access is access Data_Block;
        
  type Vector_Ptr is access Vector;
        
  type Meta_Data is record
    Vector_View : Vector_Ptr;
    Block_View  : Data_Block_Access;
  end record;
        
end;    
