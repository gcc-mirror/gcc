package Loop_Optimization1 is

   type Number is range 0 .. 127;

   type Group_List is array (Positive range <>) of Number;

   subtype Index is Natural range 1 .. 5;

   function Groups (T : Integer) return Group_List;
   pragma Import (Ada, Groups);
 
   type Group_Chain (Length : Index := 1) is record
      Groups : Group_List(1 .. Length);
   end record;

   type Group_Chain_List is array (Positive range <>) of Group_Chain;

   function Group_Chains (T : Integer) return Group_Chain_List;
   pragma Import (Ada, Group_Chains);

   type R (I : Boolean) is null record;
 
   type R_Access is access R;

   type R_List is array (Positive range <>) of R_Access;

   type R_List_Access is access R_List;

   type D is record
      L : R_List_Access;
   end record;

   procedure Create (A : in out D; Val : Integer);

end Loop_Optimization1;
