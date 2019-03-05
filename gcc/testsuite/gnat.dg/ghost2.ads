package Ghost2 is
   type Val_Entry is (A, B, C, D);

   function Transition_Valid (L : Val_Entry; R : Val_Entry) return Boolean
   is ((L = B and R = C) or
       (L = C and R = C) or
       (L = C and R = D) or
       (L = D and R = B))
     with Ghost;

   procedure Set;

   type Val_Array is array (1 .. 5) of Val_Entry;
end Ghost2;
