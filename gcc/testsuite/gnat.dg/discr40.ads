pragma Assertion_Policy(Check);

package Discr40 is

   subtype Element is Integer;

   type Vector is array (Positive range <>) of Element;

   type Stack (Max_Length : Natural) is
   record
      Length : Natural;
      Data : Vector (1 .. Max_Length);
   end record;

   function Not_Full (S : Stack) return Boolean is
      (S.Length < S.Max_Length);

    procedure Push (S: in out Stack; E : Element)
         with Pre => Not_Full(S),   -- Precodition
              Post =>             -- Postcondition
                 (S.Length = S'Old.Length + 1) and
                 (S.Data (S.Length) = E) and
                 (for all J in 1 .. S'Old.Length =>
                     S.Data(J) = S'Old.Data(J));

end Discr40;
