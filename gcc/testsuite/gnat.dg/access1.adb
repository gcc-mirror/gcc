-- { dg-do compile }

procedure access1 is
   protected Objet is
      procedure p;
   end Objet;
   protected body Objet is
      procedure p is
      begin
         null;
      end p;
   end Objet;
   type wrapper is record
      Ptr : access protected procedure := Objet.p'access;
   end record;
   It : wrapper;
   PP : access protected procedure;
begin
   PP := Objet.p'access;
   PP.all;
   It.Ptr.all;
end;
