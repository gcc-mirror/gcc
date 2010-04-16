package Wide_Boolean_Pkg is

   type TBOOL is new BOOLEAN;
   for  TBOOL use (FALSE => 0, TRUE => 1);
   for  TBOOL'SIZE use 8;

   type TUINT32 is mod (2 ** 32);
   for  TUINT32'SIZE use 32;

   type TREC is
      record
         H : TUINT32;
         B : TBOOL;
      end record;
   for TREC use
      record
         H at 0 range 0..31;
         B at 4 range 0..31;
      end record;

   procedure Modify (LH : in out TUINT32; LB : in out TBOOL);
   pragma export(C, Modify, "Modify");

end Wide_Boolean_Pkg;
