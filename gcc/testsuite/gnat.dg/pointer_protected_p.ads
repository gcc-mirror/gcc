package pointer_protected_p is
   type T;
   
   type Ptr is access protected procedure (Data : T);
   
   type T is record
      Data : Ptr;
   end record;
end pointer_protected_p;
