package Discr9 is

   type IArr is Array (Natural range <>) of Integer;
   type CArr is Array (Natural range <>) of Character;

   type Var_R (D1 : Boolean; D2 : Boolean) is record
      case D1 is
	 when True =>
	    L : IArr (1..4);
	    M1, M2 : CArr (1..16);
	 when False =>
	    null;
      end case;
   end record;

   type R (D1 : Boolean; D2 : Boolean) is record
      Field : Var_R (D1, D2);
   end record;

   procedure Proc (From : in R; To : out R);

end Discr9;
