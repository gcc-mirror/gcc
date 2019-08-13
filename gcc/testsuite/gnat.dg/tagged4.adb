--  { dg-do compile }

procedure Tagged4 is
   type T0 is tagged null record;

   generic
      type F1 is tagged private;
   procedure Gen1;

   procedure Gen1 is
      type Inst1 is new F1 with null record;  --  { dg-error "ancestor type \"F1\" is formal type of enclosing generic unit \\(RM 3\\.9\\.1 \\(4\\/2\\)\\)" }
   begin
      null;
   end Gen1;

   generic
      type F2 is interface;
   procedure Gen2;

   procedure Gen2 is
      type Inst2 is new T0 and F2 with null record;  --  { dg-error "ancestor type \"F2\" is formal type of enclosing generic unit \\(RM 3\\.9\\.1 \\(4\\/2\\)\\)" }
   begin
      null;
   end Gen2;

begin
   null;
end Tagged4;
