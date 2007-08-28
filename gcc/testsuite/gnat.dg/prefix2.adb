--  { dg-do compile }

    package body prefix2 is
       procedure Positionne (Objet : in out Instance; X, Y :  Coordonnee) is
       begin
          Objet.X := X;
          Objet.Y := Y;
       end Positionne;
       function RetourneX (Objet : in Instance) return Coordonnee is
       begin
          return Objet.X;
       end RetourneX;
       function RetourneY (Objet : in Instance) return Coordonnee is
       begin
          return Objet.Y;
       end RetourneY;
       procedure Affiche (Objet : in Class; EstVisible : Boolean) is 
       begin
          if EstVisible then
             Objet.Allume;
          else
             Objet.Eteins;
          end if;
       end Affiche;
       procedure Deplace (Objet : in out Class; DX, DY : Coordonnee) is
       begin
          Objet.Affiche (False);  -- erreur
          Objet.Positionne (Objet.X + DX, Objet.Y + DY);
          Objet.Affiche (True);   -- erreur
       end Deplace;
    end prefix2;
