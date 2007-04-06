package ext1 is
    type I_Smiley is interface;
    procedure Set_Mood (Obj : out I_Smiley) is abstract;
--      
    type Smiley (Max : Positive) is abstract new I_Smiley with record
       S : String (1 .. Max);
    end record; 
--      
    type Regular_Smiley is new Smiley (3) with null record; 
    overriding
    procedure Set_Mood (Obj : out Regular_Smiley);
end ext1;
