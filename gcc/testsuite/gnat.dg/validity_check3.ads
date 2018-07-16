package Validity_Check3 is
   procedure Call_All;

   type Rec_1 is private;
   procedure Proc_Vis_Rec_1 (Param : Rec_1);

   type Rec_2 (<>) is private;
   procedure Proc_Vis_Rec_2 (Param : Rec_2);

   type Rec_3 (<>) is private;
   procedure Proc_Vis_Rec_3 (Param : Rec_3);

   type Rec_4 (Discr : Integer) is private;
   procedure Proc_Vis_Rec_4 (Param : Rec_4);

   type Tag_1 is tagged private;
   procedure Proc_Vis_Tag_1 (Param : Tag_1);
   procedure Proc_Vis_CW_1  (Param : Tag_1'Class);

   type Tag_2 (<>) is tagged private;
   procedure Proc_Vis_Tag_2 (Param : Tag_2);
   procedure Proc_Vis_CW_2  (Param : Tag_2'Class);

   type Tag_3 (<>) is tagged private;
   procedure Proc_Vis_Tag_3 (Param : Tag_3);
   procedure Proc_Vis_CW_3  (Param : Tag_3'Class);

   type Tag_4 (Discr : Integer) is tagged private;
   procedure Proc_Vis_Tag_4 (Param : Tag_4);
   procedure Proc_Vis_CW_4  (Param : Tag_4'Class);

   type Tag_5 is new Tag_1 with private;
   procedure Proc_Vis_Tag_5 (Param : Tag_5);
   procedure Proc_Vis_CW_5  (Param : Tag_5'Class);

   type Tag_6 is new Tag_4 with private;
   procedure Proc_Vis_Tag_6 (Param : Tag_6);
   procedure Proc_Vis_CW_6  (Param : Tag_6'Class);

private
   type Rec_1 is record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Rec_1 (Param : Rec_1);

   type Rec_2 is record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Rec_2 (Param : Rec_2);

   type Rec_3 (Discr : Integer) is record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Rec_3 (Param : Rec_3);

   type Rec_4 (Discr : Integer) is record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Rec_4 (Param : Rec_4);

   type Tag_1 is tagged record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Tag_1 (Param : Tag_1);
   procedure Proc_Priv_CW_1  (Param : Tag_1'Class);

   type Tag_2 is tagged record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Tag_2 (Param : Tag_2);
   procedure Proc_Priv_CW_2  (Param : Tag_2'Class);

   type Tag_3 (Discr : Integer) is tagged record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Tag_3 (Param : Tag_3);
   procedure Proc_Priv_CW_3  (Param : Tag_3'Class);

   type Tag_4 (Discr : Integer) is tagged record
      Comp_1 : Integer;
      Comp_2 : Boolean;
   end record;

   procedure Proc_Priv_Tag_4 (Param : Tag_4);
   procedure Proc_Priv_CW_4  (Param : Tag_4'Class);

   type Tag_5 is new Tag_1 with record
      Comp_3 : Integer;
      Comp_4 : Boolean;
   end record;

   procedure Proc_Priv_Tag_5 (Param : Tag_5);
   procedure Proc_Priv_CW_5  (Param : Tag_5'Class);

   type Tag_6 is new Tag_4 with record
      Comp_3 : Integer;
      Comp_4 : Boolean;
   end record;

   procedure Proc_Priv_Tag_6 (Param : Tag_6);
   procedure Proc_Priv_CW_6  (Param : Tag_6'Class);
end Validity_Check3;
