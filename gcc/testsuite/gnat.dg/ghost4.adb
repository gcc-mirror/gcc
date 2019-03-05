pragma Restrictions (No_Secondary_Stack);

procedure Ghost4 is

   procedure Dummy with Ghost is
      function Slice (S : String) return String is
         (S (S'First .. S'First + 3));

      X : String := Slice ("hello");
   begin
         null;
   end Dummy;
begin
   Dummy;
end Ghost4;
