package Deques is

    type Deque (<>) is tagged limited private;
    function Create return Deque;
    procedure Pop (D : access Deque);

    type Sequence is limited interface;
    type P_Deque is new Deque and Sequence with private;
    function Create return P_Deque;

private
    type Deque is tagged limited null record;
    type P_Deque is new Deque and Sequence with null record;
end Deques;
