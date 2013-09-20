-- { dg-do run }
-- { dg-options "-O" }

with Opt27_Pkg;

procedure Opt27 is

    type Rec_T is record
        A, B, C, D, E : Integer;
    end record;

    package List is new Opt27_Pkg (Rec_T);

    My_List : List.List_T;

    function Is_Match (Element : Rec_T; Template : Integer) return Boolean is
    begin
        return (Element.C = Template);
    end;

    function Find_Int is new List.Find_Elem (Integer, Is_Match);

    Node : List.Node_T := Find_Int (10, My_List);

begin
    if not List.Is_Null (Node) then
        raise Program_Error;
    end if;
end;
