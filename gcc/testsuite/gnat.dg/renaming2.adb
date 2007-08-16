--  { dg-do run }
--  { dg-options "-gnatws" }

with Text_IO;
procedure renaming2 is
    type RealNodeData;
    type RefRealNodeData is access RealNodeData;

    type ExpressionEntry;
    type RefExpression is access ExpressionEntry;

    type RefDefUseEntry is access Natural;
    
    type ExpressionEntry is
    record
        Number : RefDefUseEntry;
        Id     : Integer;
    end record;
   
    type RealNodeData is
    record
        Node   : RefExpression;
        Id     : Integer; 
    end record;
            
    for ExpressionEntry use
    record
        Number at 0 range  0 .. 63;
        Id     at 8 range  0 .. 31;
    end record ;
        
    for RealNodeData use
    record
        Node   at  0 range  0 .. 63;
        Id     at 8 range  0 .. 31;
    end record ;
        
    U_Node : RefDefUseEntry := new Natural'(1); 
    E_Node : RefExpression := new ExpressionEntry'(Number => U_Node,
                                                   Id => 2);
    R_Node : RefRealNodeData := new RealNodeData'(Node => E_Node, 
                                                   Id => 3);
        
    procedure test_routine (NodeRealData : RefRealNodeData)
    is  
        OldHead   : RefDefUseEntry renames NodeRealData.all.Node.all.Number;
        OldHead1  : constant RefDefUseEntry := OldHead;
    begin
      NodeRealData.all.Node := new ExpressionEntry'(Number => null, Id => 4);
      declare                                                   
        OldHead2 : constant RefDefUseEntry := OldHead;
      begin
        if OldHead1 /= OldHead2
        then
          Text_IO.Put_Line (" OldHead changed !!!");
        end if;
      end;
    end;
begin
  test_routine (R_Node);
end;
