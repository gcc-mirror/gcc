package Ghost5_Parent is

   type Priv is private;

   package Nested with Ghost is
      function Func1 (X : Priv) return Boolean is (True); -- Error flagged here
      function Func2 (X : Priv) return Boolean is (False);
   end Nested;

private

    type Priv is new Integer;

end Ghost5_Parent;
