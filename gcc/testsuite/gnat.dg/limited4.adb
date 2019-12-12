--  { dg-do compile }
procedure Limited4 is
    TBD_Error : exception;

    type Lim_Rec is limited record
        A : Integer;
        B : Boolean;
    end record;

    type Lim_Tagged is tagged limited record
        R : Lim_Rec;
        N : Natural;
    end record;

    type Lim_Ext is new Lim_Tagged with record
       G : Natural;
    end record;

    --  a) initialization expression of a CW object_declaration

    Obj1 : Lim_Tagged'Class := (raise TBD_Error);
    Obj2 : Lim_Tagged'Class := Lim_Tagged'Class'(raise TBD_Error);

    --  b) initialization expression of a CW component_declaration

    type Rec is record
       Comp01 : Lim_Tagged'Class := (raise TBD_Error);
       Comp02 : Lim_Tagged'Class := Lim_Tagged'Class'((raise TBD_Error));
    end record;

    --  c) the expression of a record_component_association

    Obj : Lim_Tagged := (R => raise TBD_Error, N => 4);

    --  d) the expression for an ancestor_part of an extension_aggregate

    Ext1 : Lim_Ext := ((raise TBD_Error) with G => 0);
    Ext2 : Lim_Ext := (Lim_Tagged'(raise TBD_Error) with G => 0);

    --  e) default_expression or actual parameter for a formal object of
    --     mode in

    function Do_Test1 (Obj : Lim_Tagged) return Boolean is
    begin
       return True;
    end;

    function Do_Test2
      (Obj : Lim_Tagged := (raise TBD_Error)) return Boolean is
    begin
       return True;
    end;

    Check : Boolean;
begin
    Check := Do_Test1 (raise TBD_Error);
    Check := Do_Test2;
end;