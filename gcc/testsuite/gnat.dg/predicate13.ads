--  { dg-do compile }
generic
package Predicate13 is

    function Valid return Boolean is
    (True);

    function Foo return Boolean is
    (True);

    type State_Type is (Valid, Invalid);
    type Context_Type is private;

    private

    type Context_Type is
    record
    State : State_Type;
    end record  with Dynamic_Predicate => (State = Valid);

    procedure Dummy;

end Predicate13;
