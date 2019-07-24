package Warn21 is

   type Set is new Integer;

   function "<=" (Left : Set; Right : Set) return Boolean;

   function "=" (Left : Set; Right : Set) return Boolean with
     Post   => "="'Result = (Left <= Right and Right <= Left);

   procedure Foo;

private

   function "<=" (Left : Set; Right : Set) return Boolean is (True);
   function "=" (Left : Set; Right : Set) return Boolean is
      (Left <= Right and Right <= Left);

end Warn21;
