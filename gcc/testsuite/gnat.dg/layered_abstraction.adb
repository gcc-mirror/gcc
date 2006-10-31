package body  Layered_Abstraction is
    Z : P1.T := P2.Obj;  -- Both P1.T and P2.Obj are visible because 
                         -- they were not specified in the formal package.
                         -- Note that P2.T is not visible since it
                         -- is required to match P1.T

    use P1;              --  to make equality immediately visible 
    Yes_Again : Boolean := P1.Obj2 = P2.Obj2;
end Layered_Abstraction;
