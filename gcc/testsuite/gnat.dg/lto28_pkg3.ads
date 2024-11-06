package Lto28_Pkg3 is

   type Discr_Type is (P, Q);

   type Rec (Discr : Discr_Type) is record
      case Discr is
         when Q =>
            A : Duration := 0.0;
            B : Duration := 0.0;
         when P =>
            null;
      end case;
   end record;

   subtype Q_Rec is Rec (Q);

   Default_Q_Rec : constant Q_Rec := (Discr => Q, others => <>);

end Lto28_Pkg3;
