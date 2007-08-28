
    package prefix2 is
       type Coordonnee is range -100 .. 100;
       type Instance is abstract tagged private;
       subtype Class is Instance'Class;
       procedure Positionne (Objet : in out Instance; X, Y :  Coordonnee);
       function RetourneX (Objet : in Instance) return Coordonnee;
       function RetourneY (Objet : in Instance) return Coordonnee;
       procedure Allume (Objet : in Instance) is abstract;
       procedure Eteins (Objet : in Instance) is abstract;
       procedure Affiche (Objet : in Class; EstVisible : Boolean);
       procedure Deplace (Objet : in out Class; DX, DY : Coordonnee);
    private
       type Instance is abstract tagged record
          X, Y : Coordonnee := 0;
       end record;
    end;
