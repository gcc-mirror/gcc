--  { dg-do compile }
--  { dg-options "-gnata -gnateV" }

package body Validity_Check3 is
   procedure Proc_Priv_CW_1 (Param : Tag_1'Class) is begin null; end;
   procedure Proc_Priv_CW_2 (Param : Tag_2'Class) is begin null; end;
   procedure Proc_Priv_CW_3 (Param : Tag_3'Class) is begin null; end;
   procedure Proc_Priv_CW_4 (Param : Tag_4'Class) is begin null; end;
   procedure Proc_Priv_CW_5 (Param : Tag_5'Class) is begin null; end;
   procedure Proc_Priv_CW_6 (Param : Tag_6'Class) is begin null; end;

   procedure Proc_Priv_Rec_1 (Param : Rec_1) is begin null; end;
   procedure Proc_Priv_Rec_2 (Param : Rec_2) is begin null; end;
   procedure Proc_Priv_Rec_3 (Param : Rec_3) is begin null; end;
   procedure Proc_Priv_Rec_4 (Param : Rec_4) is begin null; end;

   procedure Proc_Priv_Tag_1 (Param : Tag_1) is begin null; end;
   procedure Proc_Priv_Tag_2 (Param : Tag_2) is begin null; end;
   procedure Proc_Priv_Tag_3 (Param : Tag_3) is begin null; end;
   procedure Proc_Priv_Tag_4 (Param : Tag_4) is begin null; end;
   procedure Proc_Priv_Tag_5 (Param : Tag_5) is begin null; end;
   procedure Proc_Priv_Tag_6 (Param : Tag_6) is begin null; end;

   procedure Proc_Vis_CW_1 (Param : Tag_1'Class) is begin null; end;
   procedure Proc_Vis_CW_2 (Param : Tag_2'Class) is begin null; end;
   procedure Proc_Vis_CW_3 (Param : Tag_3'Class) is begin null; end;
   procedure Proc_Vis_CW_4 (Param : Tag_4'Class) is begin null; end;
   procedure Proc_Vis_CW_5 (Param : Tag_5'Class) is begin null; end;
   procedure Proc_Vis_CW_6 (Param : Tag_6'Class) is begin null; end;

   procedure Proc_Vis_Rec_1 (Param : Rec_1) is begin null; end;
   procedure Proc_Vis_Rec_2 (Param : Rec_2) is begin null; end;
   procedure Proc_Vis_Rec_3 (Param : Rec_3) is begin null; end;
   procedure Proc_Vis_Rec_4 (Param : Rec_4) is begin null; end;

   procedure Proc_Vis_Tag_1 (Param : Tag_1) is begin null; end;
   procedure Proc_Vis_Tag_2 (Param : Tag_2) is begin null; end;
   procedure Proc_Vis_Tag_3 (Param : Tag_3) is begin null; end;
   procedure Proc_Vis_Tag_4 (Param : Tag_4) is begin null; end;
   procedure Proc_Vis_Tag_5 (Param : Tag_5) is begin null; end;
   procedure Proc_Vis_Tag_6 (Param : Tag_6) is begin null; end;

   procedure Call_All is
      pragma Warnings (Off);
      Obj_Rec_1 : Rec_1;
      Obj_Rec_2 : Rec_2;
      Obj_Rec_3 : Rec_3 (3);
      Obj_Rec_4 : Rec_4 (4);
      Obj_Tag_1 : Tag_1;
      Obj_Tag_2 : Tag_2;
      Obj_Tag_3 : Tag_3 (3);
      Obj_Tag_4 : Tag_4 (4);
      Obj_Tag_5 : Tag_5;
      Obj_Tag_6 : Tag_6 (6);
      pragma Warnings (On);

   begin
      Proc_Priv_CW_1 (Obj_Tag_1);
      Proc_Priv_CW_2 (Obj_Tag_2);
      Proc_Priv_CW_3 (Obj_Tag_3);
      Proc_Priv_CW_4 (Obj_Tag_4);
      Proc_Priv_CW_5 (Obj_Tag_5);
      Proc_Priv_CW_6 (Obj_Tag_6);

      Proc_Priv_Rec_1 (Obj_Rec_1);
      Proc_Priv_Rec_2 (Obj_Rec_2);
      Proc_Priv_Rec_3 (Obj_Rec_3);
      Proc_Priv_Rec_4 (Obj_Rec_4);

      Proc_Priv_Tag_1 (Obj_Tag_1);
      Proc_Priv_Tag_2 (Obj_Tag_2);
      Proc_Priv_Tag_3 (Obj_Tag_3);
      Proc_Priv_Tag_4 (Obj_Tag_4);
      Proc_Priv_Tag_5 (Obj_Tag_5);
      Proc_Priv_Tag_6 (Obj_Tag_6);

      Proc_Vis_CW_1 (Obj_Tag_1);
      Proc_Vis_CW_2 (Obj_Tag_2);
      Proc_Vis_CW_3 (Obj_Tag_3);
      Proc_Vis_CW_4 (Obj_Tag_4);
      Proc_Vis_CW_5 (Obj_Tag_5);
      Proc_Vis_CW_6 (Obj_Tag_6);

      Proc_Vis_Rec_1 (Obj_Rec_1);
      Proc_Vis_Rec_2 (Obj_Rec_2);
      Proc_Vis_Rec_3 (Obj_Rec_3);
      Proc_Vis_Rec_4 (Obj_Rec_4);

      Proc_Vis_Tag_1 (Obj_Tag_1);
      Proc_Vis_Tag_2 (Obj_Tag_2);
      Proc_Vis_Tag_3 (Obj_Tag_3);
      Proc_Vis_Tag_4 (Obj_Tag_4);
      Proc_Vis_Tag_5 (Obj_Tag_5);
      Proc_Vis_Tag_6 (Obj_Tag_6);
   end Call_All;
end Validity_Check3;
