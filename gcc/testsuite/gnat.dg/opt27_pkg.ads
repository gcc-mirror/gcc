generic

    type Element_T is private;

package Opt27_Pkg is

    type Node_T is private;

    type List_T is private;

    function Is_Null (Node : in Node_T) return Boolean;

    generic

        type Template_T is private;

        with function Is_Match
                         (Element : in Element_T;
                          Template : in Template_T) return Boolean is <>;

    function Find_Elem (Template : Template_T; List : List_T) return Node_T;

private

    type Node_Rec_T;
    type Node_T is access Node_Rec_T;

    type List_T is record
        First_Node : Node_T := null;
        Last_Node : Node_T := null;
    end record;

end Opt27_Pkg;
