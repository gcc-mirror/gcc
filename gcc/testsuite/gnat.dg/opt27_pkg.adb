package body Opt27_Pkg is

    type Node_Rec_T is record
         Element : Element_T;
         Left : Node_T;
         Right : Node_T;
    end record;

    function Is_Null (Node : in Node_T) return Boolean is
    begin
        return (Node = null);
    end Is_Null;

    function Find_Elem (Template : Template_T; List : List_T) return Node_T is
        Element_Found : Boolean := False;
        Node_Walker : Node_T := null;
    begin
        Node_Walker := List.First_Node;

        while not Element_Found and (Node_Walker /= null) loop

            if Is_Match (Node_Walker.Element, Template) then
                Element_Found := True;
            else
                Node_Walker := Node_Walker.Right;
            end if;
        end loop;

        return Node_Walker;
    end;

end Opt27_Pkg;
