package Opt11 is

   type String_Ptr is access constant String;

   type Form_Type is (Qualified, Unqualified);

   type Rec is record
      N1, N2, N3 : Natural;
      Fixed : String_Ptr;
      Form : Form_Type;
      Is_Local : Boolean := True;
   end record;
   pragma Pack (Rec);

   procedure Proc;

end Opt11;
