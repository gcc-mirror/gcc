--  { dg-do run }

procedure Enum4 is

   procedure Assert (Expected, Actual : String) is
   begin
      if Expected /= Actual then
         raise Program_Error;
      end if;
   end Assert;

   procedure Test_1 is
      type Test_Enum is (Enum_1,     Enum_2);
      for Test_Enum use (Enum_1=> 8, Enum_2=> 12);

      Enum_Values : constant array (Test_Enum) of Natural := (8, 12);

      type Test_Enum_Rep is range 1..12;
      Tmp_Test_Enum_Rep : Test_Enum_Rep;
   begin
      Tmp_Test_Enum_Rep := Test_Enum'Enum_Rep (Test_Enum'First);
      Assert (" 8", Tmp_Test_Enum_Rep'Img);

      for Enum in Test_Enum loop
         Tmp_Test_Enum_Rep := Test_Enum'Enum_Rep (Enum);
         Assert (Enum_Values (Enum)'Img, Tmp_Test_Enum_Rep'Img);
      end loop;
   end Test_1;

   procedure Test_2 is
      type Test_Enum is (Enum_1);
      for Test_Enum use (Enum_1=> 2);

      type Test_Enum_Rep_Full is range 0..2;
      subtype Test_Enum_Rep_Short is
        Test_Enum_Rep_Full range 2..Test_Enum_Rep_Full'Last;

      Tmp_Test_Enum_Rep_Full  : Test_Enum_Rep_Full;
      Tmp_Test_Enum_Rep_Short : Test_Enum_Rep_Short;

   begin
      Tmp_Test_Enum_Rep_Short := Test_Enum'Enum_Rep (Test_Enum'First);
      Assert (" 2", Tmp_Test_Enum_Rep_Short'Img);

      for Enum in Test_Enum loop
         Tmp_Test_Enum_Rep_Full := Test_Enum'Enum_Rep (Enum);
         Assert (" 2", Tmp_Test_Enum_Rep_Short'Img);
      end loop;

      for Enum in Test_Enum range Test_Enum'First .. Test_Enum'Last loop
         Tmp_Test_Enum_Rep_Short := Test_Enum'Enum_Rep(Enum);  --  Test #2
         Assert (" 2", Tmp_Test_Enum_Rep_Short'Img);
      end loop;
   end Test_2;

begin
   Test_1;
   Test_2;
end;
