-- { dg-do run }

procedure aggr1 is
   package Coord is
      type T is private;
   private 
      type T is record
          A, B, C : Float;
      end record;
   end Coord;
--
   generic 
      type T is private;
   package gen is
      type Rec (Discr : Boolean := True) is record
         needs_update : Boolean;
         case Discr is
            when True => null;
            when False =>  Value : T;
         end case;
      end record;
   end gen;
--
   subtype Graph_Range is integer range 1..1665;
   type arr is array (Graph_Range) of Coord.T;
--
   package Inst is new Gen (arr);
--
   subtype Index is integer range 1 .. 1;
--
   type Graph_Node (Active : Boolean := False) is
      record
         case Active is
            when True =>
               Comp1 : Inst.Rec;
               Comp2 : Inst.Rec;
               Comp3 : Inst.Rec;
            when False =>
               Needs_Update : Boolean;
         end case;
      end record;
--   
   Null_Graph_Node : constant Graph_Node := (False, True);
   type Graph_Table_T is array (Index) of Graph_Node;
--
   Graph_Table   : Graph_Table_T := (others => (Null_Graph_Node));
   Graph_Table_1 : Graph_Table_T := (others => (False, True));
begin
   null;
end;
