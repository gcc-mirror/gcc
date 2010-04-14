package Class_Wide2 is

   type Root_1 (V : Integer) is tagged record
      null;
   end record;

   type Child is new Root_1 (1) with null record;

   type Class_Acc is access all Child'Class;

   type Grand_Child is new Child with record
      null;
   end record;

   procedure Initialize;

end Class_Wide2;
