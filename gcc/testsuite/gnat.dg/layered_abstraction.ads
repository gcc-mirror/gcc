with Layered_Abstraction_P;
generic 
    with package P1 is new Layered_Abstraction_P(<>);
    with package P2 is new Layered_Abstraction_P(T => P1.T, Obj => <>); 
package Layered_Abstraction is
    pragma Elaborate_Body;
    X : P1.T := P2.Obj;  -- Both P1.T and P2.Obj are visible because 
                         -- they were not specified in the formal package.                               -- Note that P2.T is not visible since it
                         -- is required to match P1.T

    use P1;              --  to make equality immediately visible 
    Yes : Boolean := P1.Obj2 = P2.Obj2;
end Layered_Abstraction;
