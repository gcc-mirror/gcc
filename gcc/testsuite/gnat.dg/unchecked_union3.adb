--  { dg-do compile }

procedure Unchecked_Union3 is
   type small_array is array (0 .. 2) of Integer;
   type big_array   is array (0 .. 3) of Integer;

   type small_record is record
      field1 : aliased Integer     := 0;
      field2 : aliased small_array := (0, 0, 0);
   end record;

   type big_record is record
      field1 : aliased Integer   := 0;
      field2 : aliased big_array := (0, 0, 0, 0);
   end record;

   type myUnion (discr : Integer := 0) is record
      case discr is
         when 0 =>
            record1 : aliased small_record;
         when others =>
            record2 : aliased big_record;
      end case;
   end record;

   type UU_myUnion1 is new myUnion;
   pragma Unchecked_Union (UU_myUnion1);
   pragma Convention (C, UU_myUnion1);

   procedure Convert (A : in myUnion; B : out UU_myUnion1) is
      L : UU_myUnion1 := UU_myUnion1 (A);  --  Test
   begin
      B := L;
   end Convert;

begin
   null;
end Unchecked_Union3;
