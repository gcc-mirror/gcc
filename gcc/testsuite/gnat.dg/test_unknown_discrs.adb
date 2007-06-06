--  { dg-do compile }

procedure Test_Unknown_Discrs is
   
   package Display is

      type Component_Id (<>) is limited private;

      Deferred_Const : constant Component_Id;
   
   private
      
      type Component_Id is (Clock);

      type Rec1 is record
         C : Component_Id := Deferred_Const;
      end record;

      Priv_Cid_Object : Component_Id := Component_Id'First;

      type Rec2 is record
         C : Component_Id := Priv_Cid_Object;
      end record;

      Deferred_Const : constant Component_Id := Priv_Cid_Object;
   
   end Display;

begin
   null;
end Test_Unknown_Discrs;
